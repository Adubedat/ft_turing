type letter = char
type state = string
type direction = Right | Left
type transition = {read: letter; to_state: state; write: letter; action: direction}
type md_tape = {letters: letter list; pos: int; trs: state; lread: letter} (* md = metadata *)

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
    ]);
    ("subone",
    [
        {read = '1'; to_state = "subone"; write = '1'; action = Left};
        {read = '-'; to_state = "skip"; write = '-'; action = Left}
    ]);
    ("skip",
    [
        {read = '.'; to_state = "skip"; write = '.'; action = Left};
        {read = '1'; to_state = "scanright"; write = '.'; action = Right}
    ])
]

let ac_to_str ac = 
    if ac = Right then "RIGHT" else "LEFT"

let print_transitions () =
    let rec parse_trs rd_tr_name trs_lst =
        List.iter (fun x -> match x with
           | {read=rd; to_state=st; write=wr; action=ac} -> (
                Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
                    (rd_tr_name) (rd) (st) (wr) (ac_to_str(ac));
            )
        ) trs_lst
    in
    List.iter (fun x -> parse_trs (fst x) (snd x)) transitions

let print_intro =
    Printf.printf "\t\t\t\t{--[  %s  ]--}\n" (name);
    print_string "Alphabet : [ "; Print.char_list (alphabet); print_endline " ]";
    print_string "States : [ "; Print.str_list (states); print_endline " ]";
    Printf.printf "Initial : %s\n" (initial);
    print_string "Finals : [ "; Print.str_list (finals); print_endline " ]";
    print_transitions ();
    print_endline "-------------------------------------------------------------"

let launch_tape =
    let explode s =
        let rec exp i l =
            if i < 0 then l else exp (i - 1) (s.[i] :: l)
        in exp (String.length s - 1) []
    in
    let get_tape letters pos transition read_letter =
        {
            letters = letters;
            pos = pos;
            trs = transition;
            lread = read_letter
        }
    in
    let print_transition tape =
        let parse_trs trs_lst =
            List.iter (fun x -> match x with
                | {read=rd; to_state=st; write=wr; action=ac} when rd = tape.lread -> (
                    Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
                        (tape.trs) (rd) (st) (wr) (ac_to_str(ac))
                )
                | {read=rd; to_state=st; write=wr; action=ac} -> ()
                ) trs_lst
        in
        List.iter (fun x -> if (fst x) = tape.trs then parse_trs (snd x)) transitions
    in
    let print_tape tape =
        print_char '[';
        List.iteri (fun i x -> if i = tape.pos then Printf.printf "<%c>" (x)
            else print_char x) tape.letters;
        print_string "..................] ";
        print_transition tape
    in
    let get_next_transition tape =
        let trs_lst = List.find (fun x -> (fst x) = tape.trs) transitions in
        let trs = List.find (fun x -> x.read = tape.lread) (snd trs_lst) in
        trs.to_state
    in
    let get_letter_to_wr tape =
        let trs_lst = List.find (fun x -> (fst x) = tape.trs) transitions in
        let trs = List.find (fun x -> x.read = tape.lread) (snd trs_lst) in
        trs.write
    in
    let get_pos tape =
        let trs_lst = List.find (fun x -> (fst x) = tape.trs) transitions in
        let trs = List.find (fun x -> x.read = tape.lread) (snd trs_lst) in
        if trs.action = Right then tape.pos + 1 else tape.pos - 1
    in
    let get_letters tape wr_letter =
        List.mapi (fun i x -> if i = tape.pos then wr_letter else x) tape.letters
    in
    let init_tape = get_tape (explode Sys.argv.(1)) 0 initial (List.nth (explode Sys.argv.(1)) 0) in
    let rec write_tape tape =
        print_tape tape;
        let next_trs = get_next_transition tape in
        if next_trs = (List.hd finals) then
            exit 0;
        let letter_to_wr = get_letter_to_wr tape in
        let pos = get_pos tape in
        let tape_letters = get_letters tape letter_to_wr in
        write_tape (get_tape tape_letters pos next_trs (List.nth tape_letters pos))
    in
    write_tape init_tape
