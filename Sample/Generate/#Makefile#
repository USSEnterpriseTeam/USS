all :
	ocamlfind ocamlopt -thread -package spoc,sarek,graphics -linkpkg -package sarek_syntax -syntax camlp4o -o generate_sarek.asm generate_sarek.ml


test:
	camlp4  -I +camlp4 -I `ocamlfind query spoc_external_kernels` -I `ocamlfind query sarek_syntax` -parser o -parser op -printer o kernels_int.cma -printer kernels_ext.cma generate_sarek.ml

pp:
	camlp4  -I +camlp4 -I `ocamlfind query spoc_external_kernels` -I `ocamlfind query sarek_syntax` -parser o -parser op -printer o kernels_int.cma -printer kernels_ext.cma generate_sarek.ml > generate_sarek_pp.ml
	ocamlfind ocamlopt -thread -package spoc,sarek,graphics -linkpkg -package sarek_syntax -syntax camlp4o -o generate_sarek_pp.asm generate_sarek_pp.ml

clean:
	rm -f *.asm *.byte *.cm* *pp.ml *.o *.cl
