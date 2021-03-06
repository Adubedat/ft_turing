type letter = char
type state = string
type direction = Right | Left
type tape_data = {letters: letter list; pos: int; trs: state; lread: letter}

let name = Parsing.name 
let alphabet = Parsing.alphabet
let blank = Parsing.blank
let states = Parsing.states
let initial = Parsing.initial
let finals = Parsing.finals
let transitions = Parsing.transitions

let ac_to_str ac = 
    if ac = Parsing.Right then "RIGHT" else "LEFT"

let print_transitions () =
    let rec parse_trs rd_tr_name trs_lst =
        List.iter (fun x -> match x with
           | {Parsing.read=rd; Parsing.to_state=st; Parsing.write=wr; Parsing.action=ac} -> (
                Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
                    (rd_tr_name) (rd) (st) (wr) (ac_to_str(ac));
            )
        ) trs_lst
    in
    List.iter (fun x -> parse_trs (fst x) (snd x)) transitions

let print_intro () =
    Printf.printf "\t\t\t\t{--[  %s  ]--}\n" (name);
    print_string "Alphabet : [ "; Print.char_list (alphabet); print_endline " ]";
    Printf.printf "Blank : %c\n" (blank);
    print_string "States : [ "; Print.str_list (states); print_endline " ]";
    Printf.printf "Initial : %s\n" (initial);
    print_string "Finals : [ "; Print.str_list (finals); print_endline " ]";
    print_transitions ();
    print_endline "-------------------------------------------------------------"

let fact_num n =
    let open Num in
    let one = num_of_int 1 in
    let rec fact_loop nb acc =
        if nb = one then acc
        else
            fact_loop (nb -/ one) (acc */ nb)
    in fact_loop n one

let print_time_complexity trs_count =
    let open Num in
    let one = num_of_int 1 in
    let zero = num_of_int 0 in
    let two = num_of_int 2 in
    let n = num_of_int (String.length Parsing.input) in
    let rec get_degree acc nb =
        if acc </ one then nb -/ one
        else get_degree (acc // n) (nb +/ one)
    in
    if trs_count = one then
        Printf.printf "\n\nTime complexity for n = %s and N = %s : O(1)" (string_of_num n) (string_of_num trs_count)
    else if trs_count >/ (fact_num n) then
        Printf.printf "\n\nTime complexity for n = %s and N = %s : O(n!)" (string_of_num n) (string_of_num trs_count)
    else if trs_count >/ (two **/ n) then
        Printf.printf "\n\nTime complexity for n = %s and N = %s : O(2^n)" (string_of_num n) (string_of_num trs_count)
    else if (get_degree trs_count zero >/ zero) then (
        let degree = get_degree trs_count zero in
        if degree <=/ one then
            Printf.printf "\n\nTime complexity for n = %s and N = %s : O(n)" (string_of_num n) (string_of_num trs_count)
        else
            Printf.printf "\n\nTime complexity for n = %s and N = %s : O(n^%s)" (string_of_num n) (string_of_num trs_count) (string_of_num degree)
    )
    else (
        Printf.printf "\n\nTime complexity for n = %s and N = %s : O(logn)" (string_of_num n) (string_of_num trs_count)
    )

let str_to_charlst str =
    let rec exp i lst =
        if i < 0 then lst else exp (i - 1) (str.[i] :: lst)
    in exp (String.length str - 1) []

let get_tape letters pos transition read_letter =
    {
        letters = letters;
        pos = pos;
        trs = transition;
        lread = read_letter
    }

let print_transition tape =
    let parse_trs trs_lst =
        List.iter (fun x -> match x with
            | {Parsing.read=rd; Parsing.to_state=st; Parsing.write=wr; Parsing.action=ac}
                when rd = tape.lread -> (
                    Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
                        (tape.trs) (rd) (st) (wr) (ac_to_str(ac))
            )
            | {Parsing.read=rd; Parsing.to_state=st; Parsing.write=wr; Parsing.action=ac} -> ()
            ) trs_lst
    in
    List.iter (fun x -> if (fst x) = tape.trs then parse_trs (snd x)) transitions

let print_tape tape =
    print_char '[';
    List.iteri (fun i x -> if i = tape.pos then Printf.printf "\x1b[32m%c\x1b[0m" (x)
        else print_char x) tape.letters;
    print_string "..................] ";
    print_transition tape

let get_next_transition tape =
    try
        let trs_lst = List.find (fun x -> (fst x) = tape.trs) transitions in
        let trs = List.find (fun x -> x.Parsing.read = tape.lread) (snd trs_lst) in
        trs.Parsing.to_state
    with
        Not_found -> (
            Printf.printf "\n\x1b[31mError: read %c in transition %s not found\x1b[0m\n"
                (tape.lread) (tape.trs);
            exit 1
        )

let get_letter_to_wr tape =
    let trs_lst = List.find (fun x -> (fst x) = tape.trs) transitions in
    let trs = List.find (fun x -> x.Parsing.read = tape.lread) (snd trs_lst) in
    trs.Parsing.write

let get_pos tape =
    let trs_lst = List.find (fun x -> (fst x) = tape.trs) transitions in
    let trs = List.find (fun x -> x.Parsing.read = tape.lread) (snd trs_lst) in
    if trs.Parsing.action = Parsing.Right then tape.pos + 1 else tape.pos - 1

let get_letters tape wr_letter =
    List.mapi (fun i x -> if i = tape.pos then wr_letter else x) tape.letters

let init_tape () = get_tape (str_to_charlst Parsing.input) 0 initial
    (List.nth (str_to_charlst Parsing.input) 0)

let launch_tape () =
    let trs_count = ref 0 in
    let rec write_tape tape =
        print_tape tape;
        incr trs_count;
        let next_trs = get_next_transition tape in
        let letter_to_wr = get_letter_to_wr tape in
        let pos = get_pos tape in
        let tape_letters = get_letters tape letter_to_wr in
        if List.exists (fun x -> x = next_trs) finals then (
            if List.length tape_letters = pos then (
                let tape_letters = tape_letters @ ['.'] in
                let final_tape = get_tape tape_letters pos next_trs (List.nth tape_letters pos) in
                print_tape final_tape
            )
            else if List.length tape_letters = 1 && pos = -1 then (
                let tape_letters = '.' :: tape_letters in
                let final_tape = get_tape tape_letters 0 next_trs (List.nth tape_letters 0) in
                print_tape final_tape
            )
            else (
                let final_tape = get_tape tape_letters pos next_trs (List.nth tape_letters pos) in
                print_tape final_tape;
            );
            if Sys.argv.(1) = "-O" then
                print_time_complexity (Num.num_of_int !trs_count);
            print_char '\n'; exit 0
        )
        else (
            if pos = -1 then (
                let tape_letters = '.' :: tape_letters in
                write_tape (get_tape tape_letters 0 next_trs (List.nth tape_letters 0))
            )
            else if pos = (List.length tape_letters) then (
                let tape_letters = tape_letters @ ['.'] in
                write_tape (get_tape tape_letters pos next_trs (List.nth tape_letters pos))
            )
            else
                write_tape (get_tape tape_letters pos next_trs (List.nth tape_letters pos))
        )
    in
    write_tape (init_tape ())

let get_tape_trs tape_init =
    let trs_count = ref 0 in
    let rec tape_move tape =
        incr trs_count;
        let next_trs = get_next_transition tape in
        let letter_to_wr = get_letter_to_wr tape in
        let pos = get_pos tape in
        let tape_letters = get_letters tape letter_to_wr in
        if List.exists (fun x -> x = next_trs) finals then
            float_of_int !trs_count
        else (
            if pos = -1 then (
                let tape_letters = '.' :: tape_letters in
                tape_move (get_tape tape_letters 0 next_trs (List.nth tape_letters 0))
            )
            else if pos = (List.length tape_letters) then (
                let tape_letters = tape_letters @ ['.'] in
                tape_move (get_tape tape_letters pos next_trs (List.nth tape_letters pos))
            )
            else
                tape_move (get_tape tape_letters pos next_trs (List.nth tape_letters pos))
        )
    in
    tape_move (tape_init)

(*open Lymp
let interpreter = "/Users/rporcon/.brew/bin/python3"
let py = init "."
let polyfit = get_module py "polyfit"

let linear_regression () =
    let x_array = Array.make_float 5 in
    let fx_array = Array.make_float 5 in
    if Parsing.name = "unary_add" then (
        let tm_input = "1+1=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(0) <- 4.;
        fx_array.(0) <- fx;

        let tm_input = "111+111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(1) <- 8.;
        fx_array.(1) <- fx;

        let tm_input = "1111111+1111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial
            (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(2) <- 16.;
        fx_array.(2) <- fx;

        let tm_input = "111111111111111+111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(3) <- 32.;
        fx_array.(3) <- fx;

        let tm_input = "1111111111111111111111111111111+1111111111111111111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(4) <- 64.;
        fx_array.(4) <- fx
    )
    else if Parsing.name = "is_palindrome" then (
        let tm_input = "111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(0) <- 4.;
        fx_array.(0) <- fx;

        let tm_input = "1111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(1) <- 8.;
        fx_array.(1) <- fx;

        let tm_input = "111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(2) <- 16.;
        fx_array.(2) <- fx;

        let tm_input = "1111111111111111111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(3) <- 32.;
        fx_array.(3) <- fx;

        let tm_input = "111111111111111111111111111111111111111111111111111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(4) <- 64.;
        fx_array.(4) <- fx
    )
    else if Parsing.name = "is_same_power" then (
        let tm_input = "0011=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(0) <- 5.;
        fx_array.(0) <- fx;

        let tm_input = "00001111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(1) <- 9.;
        fx_array.(1) <- fx;

        let tm_input = "000000000111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(2) <- 19.;
        fx_array.(2) <- fx;

        let tm_input = "00000000000000000001111111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(3) <- 39.;
        fx_array.(3) <- fx;

        let tm_input = "000000000000000000000000000000000000000111111111111111111111111111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(4) <- 79.;
        fx_array.(4) <- fx
    )
    else if Parsing.name = "is_even" then (
        let tm_input = "0000=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(0) <- 5.;
        fx_array.(0) <- fx;

        let tm_input = "00000000=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(1) <- 9.;
        fx_array.(1) <- fx;

        let tm_input = "000000000000000000=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(2) <- 19.;
        fx_array.(2) <- fx;

        let tm_input = "00000000000000000000000000000000000000=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(3) <- 39.;
        fx_array.(3) <- fx;

        let tm_input = "000000000000000000000000000000000000000000000000000000000000000000000000000000=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(4) <- 79.;
        fx_array.(4) <- fx
    )
    else if Parsing.name = "universal_tm" then (
        let tm_input = "0|[q0d1s0w1ar[q0d+s1w1ar[q1d1s1w1ar[q1d=s2w_al[q2d1shw_ar#>1+1=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(0) <- 63.;
        fx_array.(0) <- fx;

        let tm_input = "0|[q0d1s0w1ar[q0d+s1w1ar[q1d1s1w1ar[q1d=s2w_al[q2d1shw_ar#>111+111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(1) <- 67.;
        fx_array.(1) <- fx;

        let tm_input = "0|[q0d1s0w1ar[q0d+s1w1ar[q1d1s1w1ar[q1d=s2w_al[q2d1shw_ar#>1111111+1111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(2) <- 75.;
        fx_array.(2) <- fx;

        let tm_input = "0|[q0d1s0w1ar[q0d+s1w1ar[q1d1s1w1ar[q1d=s2w_al[q2d1shw_ar#>111111111111111+111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(3) <- 91.;
        fx_array.(3) <- fx;

        let tm_input = "0|[q0d1s0w1ar[q0d+s1w1ar[q1d1s1w1ar[q1d=s2w_al[q2d1shw_ar#>1111111111111111111111111111111+1111111111111111111111111111111=" in
        let init_tape = get_tape (str_to_charlst tm_input) 0 initial (List.nth (str_to_charlst tm_input) 0) in
        let fx = get_tape_trs init_tape in
        x_array.(4) <- 123.;
        fx_array.(4) <- fx
    );

    for i = 0 to 4 do
        Printf.printf "x: %f, fx: %f\n" x_array.(i) fx_array.(i)
    done;
    let x_lst = [Pyfloat x_array.(0); Pyfloat x_array.(1); Pyfloat x_array.(2);
        Pyfloat x_array.(3); Pyfloat x_array.(4)] in
    let fx_lst = [Pyfloat fx_array.(0); Pyfloat fx_array.(1); Pyfloat fx_array.(2);
        Pyfloat fx_array.(3); Pyfloat fx_array.(4)] in
    let poly_pylst = get_list polyfit "my_polyfit" [Pylist x_lst; Pylist fx_lst; Pyint 2] in
    let poly_floatlst = List.map (fun (Pyfloat x) -> x) poly_pylst in
    let a = List.nth poly_floatlst 0 in
    let b = List.nth poly_floatlst 1 in
    let c = List.nth poly_floatlst 2 in
    let f x = a *. (x ** 2.) +. b *. x +. c in
    let h = Owl.Plot.create ("graph_" ^ Parsing.name ^ ".png") in
    Owl.Plot.set_title h ("Function: f(x) = " ^ (string_of_float a) ^ "x?? + " ^ (string_of_float b) ^ "x + " ^ (string_of_float c));
    Owl.Plot.set_xlabel h "Input length";
    Owl.Plot.set_ylabel h "Operation number";
    Owl.Plot.plot_fun ~h f 1. 100.;
    Owl.Plot.output h;
    close py
*)
