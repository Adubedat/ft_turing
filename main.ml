open Lymp

(* change "python3" to the name of your interpreter *)
let interpreter = "/Users/rporcon/.brew/bin/python3"
let py = init "."
let polyfit = get_module py "polyfit"

let () =
    let x_array = [Pyfloat 0.; Pyfloat 2.] in
    let y_array = [Pyfloat 2.; Pyfloat 4.] in
    let res = get_list polyfit "my_polyfit" [Pylist x_array; Pylist y_array; Pyint 1] in
    let my_lst = List.map (fun (Pyfloat x) -> x) res in
    Print.float_list my_lst;
	close py

(* module Tm = Turing_machine *)

(* let () = *)
(*     Tm.print_intro (); *)
(*     if Sys.argv.(1) = "-Olr" then *)
(*         Tm.linear_regression () *)
(*     else *)
(*         Tm.launch_tape () *)
