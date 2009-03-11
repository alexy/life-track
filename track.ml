open Printf
let leprintf format = eprintf (format ^^ "%!")

module D=Dataframe
(* type track = Dataframe.track = {
person: int;
cell: int;
st: int64;
dt: int;
} 
-- instead of prefixless record fields, 
-- now they can be referred to as D.person, D.cell, etc. *)

module G=Graph.Pack.Graph  

(* NB array parameter is passed by mutable value: 
  let arf a = Array.iteri (fun i _ -> a.(i) <- a.(i) + 1) a *)
  
let find_person_starts a =
  let scan (i,p,li) row = 
    let q = row.D.person in
    let i' = i + 1 in
    if p <> q then (i',q,i::li) else (i',q,li) in
  (* a.(0)-1 <> a.(0) -- initiating recording with 0! :) *)
  let (_,_,li) = Array.fold_left scan (0,a.(0).D.person-1,[]) a in
  let li = (Array.length a) :: li in
  List.rev li

(* NB rewrite as fold_right not to reverse? *)
let person_segments = function
  | x::xs -> 
    let _, res = List.fold_left (fun r a -> (a, (fst r, a-1)::(snd r))) (x,[]) xs
  in List.rev res
  | _ -> []  
    
    
let show_segments segs =
  let show_pair (a,b) = printf "%d..%d\n" a b in
  List.iter show_pair segs


let list_trajectories a starts =
  let one_segment = function (left,right) ->
    let rec go acc i =
      if i >= left then
        go (a.(i).D.cell::acc) (i-1)
      else acc in
      go [] right in
    List.map (fun seg -> one_segment)

(* we could add persons to edges, or have edges as a list of lists by person,
  but here it's just a plain flat list of all edges *)
let df_edges a =
  let step (prev_person, prev_cell, edges) row =
    let person = row.D.person in
    let cell = row.D.cell in
    if person = prev_person then
      let edge = (prev_cell, cell) in
      (prev_person, prev_cell, edge::edges)
    else
      (person, cell, edges)
  in
  Array.fold_left step (-1,-1,[]) a
  

let graph_of_df ?(progress=0) a =
  let g = G.create () in
  let vertex_count = ref 0 in
  let edge_count = ref 0 in
  let step (prev_person, prev_vertex) row =
    let person = row.D.person in
    let cell = row.D.cell in
    let v = 
      try 
        G.find_vertex g cell
      with Not_found -> 
        let v = G.V.create cell in
        G.add_vertex g v;
        vertex_count := !vertex_count + 1;
        if progress > 0 && !vertex_count mod progress = 0 
        then leprintf "x" else ();
        v
      in
    if person = prev_person then begin
        G.add_edge g prev_vertex v;
        edge_count := !edge_count + 1;
        if progress > 0 && !edge_count mod progress = 0 
        then leprintf "." else ();
      end
    else
      if progress > 0 then leprintf "%d" person else ();
    (person, v)
  in
  (* NB we really need an empty vertex V.empty *)
  let v0 = G.V.create a.(0).D.cell in
  let _,_ = Array.fold_left step (-1,v0) a in
  g
  
  
let save_graph g filename =
  let ob = open_out_bin filename in
  Marshal.to_channel ob g [];
  let num_vertices = G.nb_vertex g in
  let num_edges = G.nb_edges g in
  leprintf "saved graph with %d vertices and %d edges to file %s\n" 
    num_vertices num_edges filename;
  close_out ob
    
  
let load_graph filename =
  let ib = open_in_bin filename in
  let (g: G.t) = Marshal.from_channel ib in
  close_in ib;
  let num_vertices = G.nb_vertex g in
  let num_edges = G.nb_edges g in
  leprintf "loaded graph with %d vertices and %d edges from file %s\n"
    num_vertices num_edges filename;
  g


let () =
  let sort_by_person_ = ref false in
  let graph_filename_ = ref "" in
  let rest_ = ref [] in
  Arg.parse
      ["-sp", Arg.Unit (fun () ->  sort_by_person_ := not !sort_by_person_), 
       " <bool>  whether to sort the dataframe by person";
       "-g", Arg.String (fun s -> graph_filename_ := s),
       " <string> filename to marshal the graph into"
      ]
      (fun s -> rest_ := s :: !rest_)
      (sprintf "usage: %s <options>" Sys.argv.(0));
    
  let sort_by_person = !sort_by_person_ in
  let graph_filename = !graph_filename_ in
  let rest = !rest_ in
    
  let df_filename =  
    if List.length rest <> 1 then
      failwith "need filename for dataframe"
    else
      (* how do we invoke composition . in ocaml, or |> *)
      List.hd rest
  in

  let df = Dataframe.load df_filename in
  (* let df_len = Array.length df in
     eprintf "loaded the dataframe indeed with %d rows\n" df_len; 
     flush stderr *)
  
  if sort_by_person then begin
      Array.sort D.compare_person_time_cell df;
      leprintf "sorted the array by person\n"
    end
  else
    leprintf "using dataframe as-is, however sorted\n";

  let  person_starts = find_person_starts df in
  printf "[%s]\n" (String.concat "; " (List.map (fun e -> string_of_int e) person_starts));
  
  let segments = person_segments person_starts in
  show_segments segments;
  
  let progress = 1000 in
  let g = graph_of_df ~progress df in
  save_graph g graph_filename
  