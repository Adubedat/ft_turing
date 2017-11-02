open Lymp

(* change "python3" to the name of your interpreter *)
let interpreter = "/Users/rporcon/.brew/bin/python3"
let py = init "."
let simple = get_module py "simple"

let () =
    let x_array = [Pyfloat 0.; Pyfloat 2.] in
    let y_array = [Pyfloat 2.; Pyfloat 4.] in
    let res = get_list simple "my_polyfit" [Pylist x_array; Pylist y_array; Pyint 2] in
    print_float (List.nth res 0);

	(* msg = simple.get_message() *)
	(* let msg = get_string simple "get_message" [] in *)
	(* let integer = get_int simple "get_integer" [] in *)
	(* let addition = get_int simple "sum" [Pyint 12 ; Pyint 10] in *)
	(* let strconcat = get_string simple "sum" [Pystr "first " ; Pystr "second"] in *)
	(* Printf.printf "%s\n%d\n%d\n%s\n" msg integer addition strconcat ; *)

	close py

(* module Tm = Turing_machine *)

(* let () = *)
(*     Tm.print_intro (); *)
(*     if Sys.argv.(1) = "-Olr" then *)
(*         Tm.linear_regression () *)
(*     else *)
(*         Tm.launch_tape () *)
