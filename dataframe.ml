open Printf
let leprintf format = eprintf (format ^^ "%!")

type track = {
  person: int;
  cell: int;
  st: int64;
  (* et: int64; -- redundant, restorable from short delta *)
  dt: int;
  }

let save dataframe filename =
  let ob = open_out_bin filename in
  Marshal.to_channel ob dataframe [];
  leprintf "saved dataframe with %d rows to file %s\n" (Array.length dataframe) filename;
  close_out ob
  
let load filename =
  let ib = open_in_bin filename in
  let (matrix: track array) = Marshal.from_channel ib in
  close_in ib;
  leprintf "loaded dataframe with %d rows from file %s\n" (Array.length matrix) filename;
  matrix

let compare_person_time_cell a b =
  if a.person <> b.person then
    compare a.person b.person
  else if a.st <> b.st then
    compare a.st b.st
  else compare a.cell b.cell
