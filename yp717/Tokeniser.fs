module Tokeniser

type Token = 
    | ID of string
    | INT of int
    | HAT
    | PLUS
    | MINUS

let regex s = System.Text.RegularExpressions.Regex(s)

let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"

// val tokenize : s:string -> Token list
let tokenise (s : string) = 
    [for x in tokenR.Match(s).Groups.["token"].Captures do
        let token = 
            match x.Value with
            | "^" -> HAT
            | "-" -> MINUS
            | "+" -> PLUS
            | s when System.Char.IsDigit s.[0] -> INT (int s)
            | s -> ID s
        yield token]