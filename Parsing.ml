(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Parsing.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/19 15:07:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/20 18:23:34 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type letter = char
type state = string
type direction = Right | Left
type transition = {read: letter; to_state: state; write: letter; action: direction}

let file_name =
    try
        Sys.argv.(1)
    with
        | exn -> ""

let json_file =
    try
        Yojson.Basic.from_file file_name
    with
        | exn -> `Null

let name =
    try
        let open Yojson.Basic.Util in
        json_file |> member "name" |> to_string
    with
        | exn -> ""

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
                            | _ -> []
                    end
                | [] -> acc
        in
        loop lst []
    end
    with
        | exn -> []

let blank =
    try 
    begin
        let open Yojson.Basic.Util in
        let c = json_file |> member "blank" |> to_string in
        match String.length c with
            | 1 -> c.[0]
            | _ -> '\x00'
    end
    with
        | exn -> '\x00'

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
        | exn -> []

let initial =
    try
    begin
        let open Yojson.Basic.Util in
        let str = json_file |> member "initial" |> to_string in
        if List.mem str states then str else ""
    end
    with
        | exn -> ""

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
                | head :: tail when List.mem head states = false -> []
                | head :: tail -> loop tail
                | [] -> lst_str
        in
        loop lst_str
    end
    with
        | exn -> []

let new_transition (obj: Yojson.Basic.json) =
    let open Yojson.Basic.Util in
    let empty_record = {read = '\x00'; to_state = ""; write = '\x00'; action = Left} in
    let read = obj |> member "read" |> to_string in
    let to_state = obj |> member "to_state" |> to_string in
    let write = obj |> member "write" |> to_string in
    let action = obj |> member "action" |> to_string in
    if String.length read = 1 && List.mem read.[0] alphabet = false then (print_endline "read element not in alphabet list"; empty_record)
    else if List.mem to_state states = false then (print_endline "to_state element not in states list"; empty_record)
    else if String.length write = 1 && List.mem write.[0] alphabet = false then (print_endline "write element not in alphabet list"; empty_record)
    else if action <> "RIGHT" && action <> "LEFT" then (print_endline "action element not well formatted"; empty_record)
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
        | exn -> []
