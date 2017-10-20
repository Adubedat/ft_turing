(* alias on Turing_machine with tm *)

let print_help () =
    print_endline "usage: ft_turing [-h] jsonfile input
    
positional arguments:
    jsonfile            json description of the machine
    
    input               input of the machine
    
optional arguments:
    -h, --help          show this help message and exit"

let rec print_char_list lst =
    match lst with
        | head :: tail -> print_char head; print_string " -> "; print_char_list tail
        | [] -> print_string "[]"

let rec print_string_list lst =
    match lst with
        | head :: tail -> print_string head; print_string " -> "; print_string_list tail
        | [] -> print_string "[]"


let () =
    try
        begin
  (*          print_endline Parsing.name;
            print_char_list Parsing.alphabet; print_char '\n';
            print_char Parsing.blank; print_char '\n'; *)
        end
    with
        | exn -> print_endline "Error"
