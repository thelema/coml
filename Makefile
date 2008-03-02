#OCAMLC=/home/thelema/bin/ocamlcvs/bin/ocamlc
OCAMLC=ocamlc

coml: coml.ml
	$(OCAMLC) -g -o coml -I +lablgtk2 unix.cma lablgtk.cma gtkInit.cmo coml.ml

coml2: coml.ml
	$(OCAMLC) -o coml2 -I +lablgtk2 lablgtk.cma gtkInit.cmo coml.ml

opt: coml.ml
	ocamlopt -o coml -I +lablgtk2 -w s lablgtk.cmxa gtkInit.cmx coml.ml

install: coml
	cp -f coml /usr/local/bin/coml

install.opt: coml.opt
	cp -f coml.opt /usr/local/bin/coml

clean:
	rm -f coml coml.opt *.cm[oix] *.o

