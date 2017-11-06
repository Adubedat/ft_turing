RESULT = turing_machine
SOURCES = Print.ml Parsing.ml Turing_machine.ml main.ml
PACKS = owl yojson num lymp
THREADS = yes
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
