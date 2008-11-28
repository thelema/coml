FLAGS=-cflags -I,+lablgtk2 -lflags -I,+lablgtk2 -libs lablgtk
OCAMLBUILD=/home/thelema/bin/ocamlcvs/bin/ocamlbuild -classic-display

byte:
	$(OCAMLBUILD) $(FLAGS) coml.byte

opt:
	$(OCAMLBUILD) $(FLAGS) coml.native

clean:
	$(OCAMLBUILD) -clean
