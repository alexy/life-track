save_tracks: utils.cmx dataframe.cmx save_tracks.ml 
	ocamlopt.opt -o $@ $^

track: utils.cmx dataframe.cmx track.ml 
	ocamlfind ocamlopt -package ocamlgraph -linkpkg -o $@ $^
	
%.cmo: %.ml 
	ocamlfind ocamlc $(DEBUG) -c $< -o $@
	
%.cmx: %.ml
	ocamlfind ocamlopt -c $< -o $@
