open Printf
let leprintf format = eprintf (format ^^ "%!")
 
(* NB: bringing record fields into scope without prefixing but leaving functions prefixed: *)
type track = Dataframe.track = {
  person: int;
  cell: int;
  st: int64;
  dt: int;
  }

let read_tracks in_filename =
  let rec scan ic li count =
    match Utils.get_line ic with
  	| Some line -> begin
      (* printf "%s\n" line; *)
  	  let r = 
  	  try
  	    Scanf.sscanf line "%d %d %Ld %Ld %d" (fun p c s _ d -> {person=p;cell=c;st=s;dt=d})
  	  with _ -> failwith (sprintf "line #%d: %s\n" count line) 
  	  in
        (* printf "person=>%d cell=>%d start=>%Ld end=>%Ld delta=>%d\n" r.person r.cell r.st r.et r.dt; *)
        if count mod 10000 = 0 then leprintf "." else ();
  	    scan ic (r::li) (count+1)
  	  end
    | None -> Array.of_list (List.rev li) in
  let ic = open_in in_filename in
  let tracks = scan ic [] 0 in
  tracks
  
let () = 
  let sort_by_person_ = ref false in
  let rest_ = ref [] in
  Arg.parse
      ["-sp", Arg.Unit (fun () ->  sort_by_person_ := not !sort_by_person_), 
       " <bool>  whether to sort the dataframe by person"
      ]
      (fun s -> rest_ := s :: !rest_)
      (sprintf "usage: %s <options>" Sys.argv.(0));
    
  let sort_by_person = !sort_by_person_ in
  let rest = List.rev !rest_ in
    
  let in_filename, df_filename =  
    if List.length rest <> 2 then
      failwith "need two filenames, from and to"
    else
      (* how do we invoke composition . in ocaml, or |> *)
      List.hd rest, List.hd ( List.tl rest )
  in

  leprintf "reading from %s => %s\n" in_filename df_filename;
    
  let tracks = read_tracks in_filename in
  
  if sort_by_person then begin
      eprintf "sorting by person";
      Array.sort Dataframe.compare_person_time_cell tracks;
      leprintf "sorted the array by person\n"; 
    end 
  else
    leprintf "leaving the array as-is, however sorted\n";
    
  Dataframe.save tracks df_filename
