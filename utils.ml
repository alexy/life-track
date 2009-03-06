let get_line ic = 
  try Some (input_line ic) 
  with End_of_file -> close_in ic; None
