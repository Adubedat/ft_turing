type letter = char
type state = string
type direction = Right | Left
type transition = {read: letter; to_state: state; write: letter; action: direction}

(* Below is an example, normally field are filled by json parsing *)
let name = "unary_sub"
let alphabet = ['1'; '.'; '-'; '=']
let blank = '.'
let states = ["scanright"; "eraseone"; "subone"; "skip"; "HALT"]
let initial = "scanright"
let finals = ["HALT"]
let transitions = [
    ("scanright",
    [
        {read = '.'; to_state = "scanright"; write = '.'; action = Right};
        {read = '1'; to_state = "scanright"; write = '1'; action = Right};
        {read = '-'; to_state = "scanright"; write = '-'; action = Right};
        {read = '='; to_state = "eraseone"; write = '.'; action = Left}
    ]);
    ("eraseone",
    [
        {read = '1'; to_state = "subone"; write = '='; action = Left};
        {read = '-'; to_state = "HALT"; write = '.'; action = Left}
    ])
]

let print_transitions () =
    let ac_to_str ac = 
        if ac = Right then "Right" else "Left"
    in
    let rec parse_trs rd_tr_name trs_lst =
        match trs_lst with
            | [] -> ()
            | head::tail -> (
                match head with 
                   | {read=rd; to_state=st; write=wr; action=ac} -> (
                        Printf.printf ("(%s, %c) -> (%s, %c, %s)\n")
                            (rd_tr_name) (rd) (st) (wr) (ac_to_str(ac));
                        parse_trs rd_tr_name tail
                    )
            )
    in
    let rec parse_trs_info trsinfo_lst =
        match trsinfo_lst with
            | [] -> ()
            | head::tail -> (
                parse_trs (fst head) (snd head);
                parse_trs_info tail
            )
    in parse_trs_info transitions

let print_intro =
    Printf.printf "\t\t\t\t\t{--[  %s  ]--}\t\t\t\t\t\n" (name);
    print_string "Alphabet : [ "; Print.char_list (alphabet); print_endline " ]";
    print_string "States : [ "; Print.str_list (states); print_endline " ]";
    Printf.printf "Initial : %s\n" (initial);
    print_string "Finals : [ "; Print.str_list (finals); print_endline " ]";
    print_transitions () (* if not () print_transitions is print before first printf above oO *)
