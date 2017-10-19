module Tm = Turing_machine

let rec print_char_list (lst: Tm.letter list) =
    match lst with
    | head::[] -> print_char head
    | [] -> ()
    | head::tail -> print_char head; print_string ", "; print_char_list tail

let print_machine =
    Printf.printf "\t\t\t\t\t[\t%s\t]\t\t\t\t\t\n" (Tm.name);
    let c = List.nth Tm.alphabet 1 in
    print_char c;
    (*print_char_list (Tm.alphabet);*)
    Printf.printf "Initial : %s\n"  (Tm.initial);
    Printf.printf "Finals : [ %s ]\n"  (Tm.initial)

let () =
    print_machine
