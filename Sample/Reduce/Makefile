all :
	ocamlfind ocamlopt -thread -package spoc,sarek,graphics -linkpkg -package sarek_syntax -syntax camlp4o -o reduce.asm reduce.ml


test:
	camlp4  -I +camlp4 -I `ocamlfind query spoc_external_kernels` -I `ocamlfind query sarek_syntax` -parser o -parser op -printer o kernels_int.cma -printer kernels_ext.cma reduce.ml

pp:
	camlp4  -I +camlp4 -I `ocamlfind query spoc_external_kernels` -I `ocamlfind query sarek_syntax` -parser o -parser op -printer o kernels_int.cma -printer kernels_ext.cma reduce.ml > reduce_pp.ml
	ocamlfind ocamlopt -thread -package spoc,sarek,graphics -linkpkg -package sarek_syntax -syntax camlp4o -o reduce_pp.asm reduce_pp.ml

clean:
	rm -f *.asm *.byte *.cm* *pp.ml *.o *.cl
