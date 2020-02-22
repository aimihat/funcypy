module Tokeniser

open System

type Token = 
    | LRB               // left round bracket
    | RRB               // right round bracket
    | LSB               // left square bracket
    | RSB               // right square bracket
    | DOT               // DOT expression (this is where i guess we will build on)
    | Other of string

// type Token = 
//     | ID of string
//     | INT of int
//     | HAT
//     | PLUS
//     | MINUS

let tokenise (str: string) : (Token list) =
    let rec recTokenise lst =
        match lst with
        | [] -> []
        | hd::tl when hd = '(' -> [LRB] @ recTokenise tl
        | hd::tl when hd = ')' -> [RRB] @ recTokenise tl
        | hd::tl when hd = '[' -> [LSB] @ recTokenise tl
        | hd::tl when hd = ']' -> [RSB] @ recTokenise tl
        | hd::tl when hd = '.' -> [DOT] @ recTokenise tl
        | _ ->
            let rec extractOther acc lst =
                match lst with
                | hd::tl when not <| List.contains hd ['(';')';'[';']';'.'] -> 
                    extractOther (acc @ [hd]) tl
                | _ -> acc, lst

            let acc, tl = extractOther [] lst
            [Other (String.Concat(acc))] @ recTokenise tl
    Seq.toList str |> recTokenise

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
