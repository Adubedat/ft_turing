module Tm = Turing_machine

let () =
    Tm.print_intro ();
    if Sys.argv.(1) = "-Olr" then
        ()
(*        Tm.linear_regression () *)
    else
        Tm.launch_tape ()
