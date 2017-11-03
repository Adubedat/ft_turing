(* Array *)
let float_array float_arr =
    let arr_len = Array.length float_arr in
    let i = ref 0 in
    while !i < arr_len do
        print_float (Array.get float_arr !i);
        print_char ',';
        incr i
    done

(* list *)
let rec char_list lst =
    match lst with
    | head::[] -> print_char head
    | [] -> ()
    | head::tail -> print_char head; print_string ", "; char_list tail

let rec str_list lst =
    match lst with
    | head::[] -> print_string head
    | [] -> print_char '\n'
    | head::tail -> print_string head; print_string ", "; str_list tail

let rec int_list lst =
    match lst with
    | head::[] -> print_int head
    | [] -> print_char '\n'
    | head::tail -> print_int head; print_string ", "; int_list tail

let rec float_list lst =
    match lst with
    | head::[] -> print_float head
    | [] -> print_char '\n'
    | head::tail -> (
        Printf.printf "%.5f, " head; float_list tail
    )

(* Tuple*)
let rec intint_tup tup =
    match tup with
    | fst, snd -> print_char '('; print_int fst; print_string ", ";
         print_int snd; print_char ')'

let rec intstr_tup tup =
    match tup with
    | fst, snd -> print_char '('; print_int fst; print_string ", ";
         print_string snd; print_char ')'

let print_help () =
    print_endline "usage: ft_turing [-h][-O] jsonfile input

positional arguments:
    jsonfile            json description of the machine

    input               input of the machine

optional arguments:
    -h, --help          show this help message and exit
    -O                  print time complexity"
