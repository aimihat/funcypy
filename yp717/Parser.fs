module Parser

open Common
open Tokeniser

// The Parser code takes a list of tokens and turns this into a parse tree which can easily be evaluated.
// The process is complicated because it must cope with function application

let print x = printfn "%A" x
exception TokenException of int * string
 
//////////////////////////////////////// AST DEFINITION ///////////////////////////////////////
type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = string

type Literal =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Tuple of Literal*Literal

type Ast =    
    | Statement of Ast    
    | Expression of Ex    
    | Function of Identifier option * Identifier * Ast
    // | Scope of Ast list option
    | Conditional of Ex * Ast * Ast option
    | Call of Ast * Ast
    // | Assign of Identifier * Ex
    | Combinator of CombinatorType
and Ex =
    | Single of Ast
    | Literal of Literal
    | Variable of Identifier
    | Arithmetic of Ex * Arithmetic * Ex
    | Comparison of Ex * Comparison * Ex
and CombinatorType = 
    | K 
    | I 
    | S

///////////////////////////////////// TOKEN PREPROCESSING /////////////////////////////////////
// Generic function that checks if token is specific operator token in list
let isSpecTokenInList (lst: string list) =
    function
    | TokSpec op -> List.contains op lst
    | _ -> false

// Checks if token is a literal value
let isLiteralTok t = 
    match t with
    | TokBoolLit _ | TokIntLit _ | TokFloatLit _ | TokStrLit _ -> true
    | _ -> false

// Checks if token is a Binary operator
let isBinaryTok = isSpecTokenInList binaryOps

// Checks if token is Arithmetic operator
let isArithmeticTok = isSpecTokenInList arithmeticOps

// token is a unary operator   
let isUnaryTok = isSpecTokenInList unaryOps

// Checks if token is Comparison operator
let isComparisonTok = isSpecTokenInList comparisonOps

/////////////////////////////////////////// PARSER ////////////////////////////////////////////

// PAP matches (head::rest) input if pred p = true
// Outputs its input unchanged
let (|TOK|_|) pred = 
    function 
    | head :: rest when pred head -> Some(head :: rest)
    | _ -> None


// and Ex =
//     | Single of Ast
//     | Literal of Literal
//     | Variable of Identifier
//     | Arithmetic of Ex * Arithmetic * Ex
//     | Comparison of Ex * Comparison * Ex

let rec (|PEXP|_|) (headIndex, lst): Option<Ast * Token list * int>  =
    match (headIndex, lst) with


    
    | PROUNDBRA(roundAst, rest, restIndex) -> 
        match (restIndex, rest) with
        | PSQUAREBRA (sqAst, rest', restIndex') -> Some(BOTHBRAEXP(roundAst, sqAst), rest', restIndex')
        | PROUNDBRA(_) -> raise(TokenException(restIndex, "Invalid token found"))
        | _ -> Some(roundAst, rest, restIndex)
    | PSQUAREBRA(ast, rest, restIndex) -> Some(ast, rest, restIndex)
    | (_, (DOT::tl)) -> Some(DOTEXP, tl, headIndex + 1)
    | _  -> None
       
and (|PROUNDBRA|_|) (headIndex, tokenList): Option<AstT3 * Token list * int>  = 
    match (LRB, tokenList) with
    | PTOKEN(rest) -> 
        let nextIndex = headIndex + 1
        match (nextIndex, rest) with
        | PEXP(ast, tail, tailIndex) ->
            match (RRB, tail) with
            | PTOKEN(rest') -> Some(ROUNDBRAEXP(ast), rest', tailIndex + 1)
            | _ -> None
        | _ -> None
    | _ -> None

and (|PSQUAREBRA|_|) (headIndex, tokenList): Option<AstT3 * Token list * int> =
    match (LSB, tokenList) with
    | PTOKEN(rest) -> 
        let nextIndex = headIndex + 1
        match (nextIndex, rest) with
        | PEXP(ast, tail, tailIndex) ->
            match (RSB, tail) with
            | PTOKEN(rest') -> Some(SQBRAEXP(ast), rest', tailIndex + 1)
            | _ -> None
        | _ -> None
    | _ -> None
        
type ParseResult = Result<AstT3, int * string>

// returns either the AST or
// Error indicating token number which did not parse and error message 
let parseT3 (tokL: Token list) : ParseResult =
    try 
        match (0, tokL) with
        | PEXP(ast, list, _) when List.isEmpty list -> Ok(ast) 
        | _ -> Error(0, "Invalid token found")
    with
    | TokenException(index, reason) -> Error(index, reason)


// type Term = 
//     | Term of int * string * int
//     | Const of int

// type Polynomial = Term list
// type TokenStream = Token list

// let tryToken (src: TokenStream) =
//     match src with 
//     | tok :: rest -> Some(tok, rest)
//     | _ -> None

// let parseIndex src = 
//     match tryToken src with
//     | Some (HAT, src) ->
//         match tryToken src with
//         | Some (INT num2, src) ->
//             num2, src
//         | _ -> failwith "expected an integer after '^'"
//     | _ -> 1, src

// let parseTerm src = 
//     match tryToken src with
//     | Some (INT num, src) ->
//         match tryToken src with
//         | Some (ID id, src) ->
//             let idx, src = parseIndex src
//             Term (num, id, idx), src
//         | _ -> Const num, src
//     | Some (ID id, src) ->
//         let idx, src = parseIndex src
//         Term(1, id, idx), src
//     | _ -> failwith "end of token stream in term"

// let rec parsePolynomial src = 
//     let t1, src = parseTerm src
//     match tryToken src with
//     | Some (PLUS, src) ->
//         let p2, src = parsePolynomial src
//         (t1 :: p2), src
//     | _ -> [t1], src

// let parse input = 
//     let src = tokenise input
//     let result, src = parsePolynomial src
//     match tryToken src with
//     | Some _ -> failwithf "unexpected input at end of token stream!"
//     | None -> result