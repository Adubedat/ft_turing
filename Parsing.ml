(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Parsing.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/19 15:07:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/24 18:19:57 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type letter = char
type state = string
type direction = Right | Left
type transition = {read: letter; to_state: state; write: letter; action: direction}

let file_name =
     try 
         let arg1 = Sys.argv.(1) in
         if arg1 = "-h" || arg1 = "--help" then
             (Print.print_help (); exit 0)
         else if Array.length Sys.argv <> 3 then
             (print_endline "Error: Two arguments expected. -h for details."; exit 0)
         else arg1
     with 
         | exn -> print_endline "Error: argv1 must be a valid json file."; exit 0

let json_file =
    try
        Yojson.Basic.from_file file_name
    with
        | Yojson.Json_error err-> print_endline "Error: Json file not well formatted."; exit 0
        | exn -> print_endline "Error: argv1 must be be a valid json file."; exit 0

let name =
    try
        let open Yojson.Basic.Util in
        json_file |> member "name" |> to_string
    with
        | exn -> print_endline "Error: Can not read name field."; exit 0

let alphabet = 
    try
    begin
        let open Yojson.Basic.Util in
        let lst = json_file |> member "alphabet" |> to_list in
        let rec loop lst acc =
            match lst with
                | head :: tail -> 
                    begin
                        let str = to_string head in
                        match String.length str with
                            | 1 -> loop tail (acc @ [str.[0]])
                            | _ -> print_endline "Error: alphabet elements must be char."; exit 0
                    end
                | [] -> acc
        in
        loop lst []
    end
    with
        | exn -> print_endline "Error: alphabet not well formatted."; exit 0

let blank =
    try 
    begin
        let open Yojson.Basic.Util in
        let c = json_file |> member "blank" |> to_string in
        match String.length c with
            | 1 -> if List.mem c.[0] alphabet then c.[0] else
                (print_endline "Error: blank must be in alphabet list."; exit 0)
            | _ -> print_endline "Error: blank must be a char."; exit 0
    end
    with
        | exn -> print_endline "Error: blank not well formatted."; exit 0

let states =
    try
    begin
        let open Yojson.Basic.Util in
        let lst = json_file |> member "states" |> to_list in
        let rec loop lst acc =
            match lst with
                | head :: tail -> loop tail (acc @ [to_string head])
                | [] -> acc
        in
        loop lst []
    end
    with
        | exn -> print_endline "Error: states not well formatted."; exit 0

let initial =
    try
    begin
        let open Yojson.Basic.Util in
        let str = json_file |> member "initial" |> to_string in
        if List.mem str states then str else 
            (print_endline "Error: initial must be in states list."; exit 0)
    end
    with
        | exn -> print_endline "Error: initial not well formatted."; exit 0

let finals =
    try
    begin
        let open Yojson.Basic.Util in
        let final_lst = json_file |> member "finals" |> to_list in
        let rec loop lst acc =
            match lst with
                | head :: tail -> loop tail (acc @ [to_string head])
                | [] -> acc
        in
        let lst_str = loop final_lst [] in
        let rec loop lst =
            match lst with
                | head :: tail when List.mem head states = false -> 
                        (print_endline "Error: finals must be a sublist of states."; exit 0)
                | head :: tail -> loop tail
                | [] -> lst_str
        in
        loop lst_str
    end
    with
        | exn -> []

let new_transition (obj: Yojson.Basic.json) =
    let open Yojson.Basic.Util in
    let read = obj |> member "read" |> to_string in
    let to_state = obj |> member "to_state" |> to_string in
    let write = obj |> member "write" |> to_string in
    let action = obj |> member "action" |> to_string in
    if String.length read = 1 && List.mem read.[0] alphabet = false then (print_endline "Error: read element must be in alphabet list"; exit 0)
    else if List.mem to_state states = false then (print_endline "Error: to_state element must be in states list"; exit 0)
    else if String.length write = 1 && List.mem write.[0] alphabet = false then (print_endline "Error: write element must be in alphabet list"; exit 0)
    else if action <> "RIGHT" && action <> "LEFT" then (print_endline "Error: action element not well formatted"; exit 0)
    else begin
        let act = if action = "RIGHT" then Right else Left in
        {read = read.[0]; to_state = to_state; write = write.[0]; action = act}
    end

let new_dictionnary ((name, json) : (string * Yojson.Basic.json)) =
    let open Yojson.Basic.Util in
    let transitions = json |> to_list in
    let rec loop lst acc =
        match lst with
            | head :: tail -> loop tail (acc @ [new_transition head])
            | [] -> (name, acc)
    in
    loop transitions []

let transitions =
    try
    begin
        let open Yojson.Basic.Util in
        let lst = json_file |> member "transitions" |> to_assoc in
        let rec loop lst acc =
            match lst with
                | head :: tail -> loop tail (acc @ [new_dictionnary head])
                | [] -> acc
        in
        loop lst []
    end
    with
        | exn -> print_endline "Error: transitions not well formatted."; exit 0

let input =
    try
    begin
        let input = Sys.argv.(2) in
        let str_to_charlst str =
            let rec exp i lst =
                if i < 0 then lst else exp (i - 1) (str.[i] :: lst)
            in exp (String.length str - 1) []
        in
        let input_lst = str_to_charlst input in
        if (not (List.for_all (fun x -> List.mem x alphabet && x <> blank) input_lst)) then
            (print_endline "Error: Input invalid, all characters must be in alphabet list without blank char."; exit 0)
        else if (String.length input) = 0 then
            (print_endline "Error: Empty input."; exit 0)
        else input
    end
    with
        | exn -> print_endline "Error: Invalid input field. -h for details"; exit 0

