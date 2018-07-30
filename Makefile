RESULT = turing_machine
SOURCES = Print.ml Parsing.ml Turing_machine.ml main.ml
PACKS = yojson num #lymp owl
THREADS = yes
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
