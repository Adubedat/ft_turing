(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Parsing.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/19 15:07:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/19 19:24:15 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

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
    let open Yojson.Basic.Util in
    json_file |> member "name" |> to_string

let alphabet = 
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

let blank =
    begin try 
        let open Yojson.Basic.Util in
        let c = json_file |> member "blank" |> to_string in
        match String.length c with
            | 1 -> c.[0]
            | _ -> '\x00'
    with
        | exn -> '\x00'
    end


