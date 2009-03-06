open Printf
(* NB: bringing record fields into scope without prefixing but leaving functions prefixed: *)
type track = Dataframe.track = {
  person: int;
  cell: int;
  st: int64;
  dt: int;
  }

let save_tracks in_filename out_filename =
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
        if count mod 10000 = 0 then begin printf "."; flush stdout; end else ();
  	    scan ic (r::li) (count+1)
  	  end
    | None -> Array.of_list (List.rev li) in
  let ic = open_in in_filename in
  let tracks = scan ic [] 0 in
  Dataframe.save tracks out_filename

let () = 
  let in_filename = Sys.argv.(1) in
  let df_filename = Sys.argv.(2) in
  printf "reading from %s => %s\n" in_filename df_filename;
  save_tracks in_filename df_filename
