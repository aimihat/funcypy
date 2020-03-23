module Helpers

open System.Collections.Generic
open System.IO


/////// Type definitions ///////
type Identifier = string

// Built-in functions and operations
type BuiltInType =
    | Arithm of ArithmeticType
    | Comp of ComparisonType
    | ListF of ListFunctionType
    | IfThenElse
and ArithmeticType = | Add | Subtract | Multiply | Divide
and ComparisonType = | Eq | Ne | Lt| Gt | Le | Ge
and ListFunctionType =
    | IsList
    | IsEmpty
    | Head
    | Tail
    | ImplodeStr
    | ExplodeStr
    | P // in Built-in 'required list'

// Literal types
type Value =
    | Bool of bool
    | Int of int
    | Double of double // NaN values will cause problems
    | String of string
    
// Abstract Syntax Tree
type Ast =
    | Lambda of Identifier * Ast
    | Call of Ast * Ast * StructureID
    | FuncDefExp of Identifier * Ast * Ast
    | Combinator of CombinatorType
    | Literal of Value
    | Variable of Identifier
    | BuiltInFunc of BuiltInType
    | Pair of Ast * Ast * StructureID// [1, 2, 3] = Pair(1, Pair(2, Pair(3, NULL))
    | Null
and CombinatorType = | I | S | K | Y
and StructureID =
    | ID of int
    | L of Ast // Literal
    
   
/////// Memoization ///////
    

let mutable IDCount = 0
let EvaluationMemo = Dictionary<StructureID*StructureID, Ast>()

let newID() =
    // Increments global count and returns a unique ID
    IDCount <- IDCount + 1
    ID IDCount

let getID node =
    match node with
    | Pair(_,_,id)
    | Call(_,_,id) -> id
    | other -> L other

// Leads to random test case fails -> not all the time
let GetMemoOrAdd IDs f node =
    // Get memoised result, if exists
    // Otherwise, saves evaluation and returns it
    f node
    (* Disabling memoisation, since tests run async and don't support global/mutable vars.
    match EvaluationMemo.TryGetValue IDs with
    | true, result -> result
    | false, _ ->
        let result = f node
        EvaluationMemo.TryAdd(IDs, result) |> ignore
        result*)

let DCall(e1, e2) =
    // Constructs a `Call` with Default ID
    Call(e1, e2, ID 0)
let NCall(e1, e2) =
    // Constructs a `Call` with New ID
    Call(e1, e2, newID())
let DPair(e1, e2) =
    // Constructs a `Pair` with Default ID
    Pair(e1, e2, ID 0)
let NPair(e1, e2) =
    // Constructs a `Pair` with Default ID
    Pair(e1, e2, newID())

    
/////// Helper functions ///////
    
// Re-used PAP that reduce boilerplate code
module PAPHelpers = 
    let (|INT|_|) node =
        match node with
        | Literal(Int y) -> Some y
        | _ -> None

    let (|DOUBLE|_|) node =
        match node with
        | Literal(Double y) -> Some y
        | _ -> None

    let (|NUM|_|) node =
        match node with
        | Literal(Int y) -> Some <| double y
        | Literal(Double y) -> Some y
        | _ -> None

    let (|STR|_|) node =
        match node with
        | Literal(String y) -> Some y
        | _ -> None

    let (|BOOL|_|) node =
        match node with
        | Literal(Bool y) -> Some y
        | _ -> None

    let rec (|CONSTORVAR|_|) node =
        match node with
        | Null -> Some <| Null
        | Literal x -> Some <| Literal x
        | BuiltInFunc B -> Some <| BuiltInFunc B
        | Combinator C -> Some <| Combinator C
        | Variable x -> Some <| Variable x
        | _ -> None
        
// F# list -> list from Pair()
let ListFromPairs (lst: Ast list): Ast =
    let rec compute l =
        match l with
        | [] -> Null
        | hd :: tail -> NPair(hd, compute tail)

    match lst with
    | [] -> NPair(Null, Null)
    | hd :: tail -> NPair(hd, compute tail)
    
// Recursively prints an AST tree
let rec PrintTree (tree: Ast): string = //TODO: review
    match tree with
    | FuncDefExp(x, body, exp) ->
        sprintf "%A = %A in (%A)" <| x <| body <| exp
    | Call(e1, e2, _) ->
        sprintf "(%s %s)" <| PrintTree e1 <| PrintTree e2
    | Lambda(bv, exp) ->
        sprintf "(λ%s.%s)" <| bv <| PrintTree exp
    | Pair(e1, e2, _) ->
        let rec printList l =
            match l with
            | Pair(e1, Null, _) -> (PrintTree e1)
            | Pair(e1, e2, _) -> (PrintTree e1) + ", " + printList e2
            | _ -> failwithf "PT001: Shouldn't happen!"
        "[" + printList (DPair(e1, e2)) + "]"
    | BuiltInFunc(IfThenElse) -> sprintf "IfThenElse"
    | Null -> "null"
    | Variable x
    | Literal(String x) -> x
    | BuiltInFunc(ListF x) -> sprintf "%A" <| x
    | BuiltInFunc(Arithm x) -> sprintf "%A" <| x
    | BuiltInFunc(Comp x) -> sprintf "%A" <| x
    | Literal(Int x) -> sprintf "%A" <| x
    | Literal(Double x) -> sprintf "%A" <| x
    | Literal(Bool x) -> sprintf "%A" <| x
    | Combinator x -> sprintf "%A" <| x

/////// Parser 
/// These types may potentially be useful but could be removed; depends on multiple expression
/// implementation
type Whitespace = 
    | Space             // ' '
    | FormFeed          // '\f'
    | LineFeed          // '\n'
    | CarriageReturn    // '\r'
    | HorizontalTab     // '\t'
    | VerticalTab       // '\v'
type Operator = 
    | LRB 
    | RRB 
    | RSB 
    | LSB 
    | IF 
    | EQUALS 
    | DEF 
    | LETREC
    | COLON
    | ELSE
    | LAMBDA 
    | COMMA
    | ARROWFUNC

type UnaryOps = NOT | NEGATE

type Token = 
    | TokLit of Value
    | TokUnaryOp of UnaryOps
    | TokSpecOp of Operator
    | TokIdentifier of Identifier
    | TokBuiltInOp of BuiltInType
    | TokWhitespace of Whitespace
    
// Loading standard library, and user code to execute.

let loadCode file =
    let baseDirectory = __SOURCE_DIRECTORY__
    let baseDirectory' = Directory.GetParent(baseDirectory)
    Path.Combine(baseDirectory'.FullName, file)
    |> System.IO.File.ReadAllText
