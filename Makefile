RESULT = turing_machine
SOURCES = Print.ml Parsing.ml Turing_machine.ml main.ml
PACKS = yojson num
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
