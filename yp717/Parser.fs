module Parser

open Tokeniser
// The Parser code takes a list of tokens and turns this into a parse tree which can easily be evaluated.
// The process is complicated because it must cope with function application

type Term = 
    | Term of int * string * int
    | Const of int

type Polynomial = Term list
type TokenStream = Token list

let tryToken (src: TokenStream) =
    match src with 
    | tok :: rest -> Some(tok, rest)
    | _ -> None

let parseIndex src = 
    match tryToken src with
    | Some (HAT, src) ->
        match tryToken src with
        | Some (INT num2, src) ->
            num2, src
        | _ -> failwith "expected an integer after '^'"
    | _ -> 1, src

let parseTerm src = 
    match tryToken src with
    | Some (INT num, src) ->
        match tryToken src with
        | Some (ID id, src) ->
            let idx, src = parseIndex src
            Term (num, id, idx), src
        | _ -> Const num, src
    | Some (ID id, src) ->
        let idx, src = parseIndex src
        Term(1, id, idx), src
    | _ -> failwith "end of token stream in term"

let rec parsePolynomial src = 
    let t1, src = parseTerm src
    match tryToken src with
    | Some (PLUS, src) ->
        let p2, src = parsePolynomial src
        (t1 :: p2), src
    | _ -> [t1], src

let parse input = 
    let src = tokenise input
    let result, src = parsePolynomial src
    match tryToken src with
    | Some _ -> failwithf "unexpected input at end of token stream!"
    | None -> result