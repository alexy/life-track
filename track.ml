open Printf
module D=Dataframe
(* type track = Dataframe.track = {
person: int;
cell: int;
st: int64;
dt: int;
} 
-- instead of prefixless record fields, 
-- now they can be referred to as D.person, D.cell, etc. *)
  
(* array parameter is passed by mutable value: 
  let arf a = Array.iteri (fun i _ -> a.(i) <- a.(i) + 1) a *)
  
let find_person_starts a =
  let scan (i,p,li) row = 
    let q = row.D.person in
    let i' = i + 1 in
    if p <> q then (i',q,i::li) else (i',q,li) in
  (* a.(0)-1 <> a.(0) -- initiating recording with 0! :) *)
  let (_,_,li) = Array.fold_left scan (0,a.(0).D.person-1,[]) a in
  List.rev li
  
let () =
  let df_filename = Sys.argv.(1) in
  let df = Dataframe.load df_filename in
  (* let df_len = Array.length df in
     eprintf "loaded the dataframe indeed with %d rows\n" df_len; 
     flush stderr *)
  
  Array.sort D.compare_person_time_cell df;
  eprintf "sorted the array by person\n"; 
  flush stderr;

  let  person_starts = find_person_starts df in
  printf "[%s]\n" (String.concat "; " (List.map (fun e -> string_of_int e) person_starts))
  
    