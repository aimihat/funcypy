module Helper

type Identifier = string

// Built-in functions and operations
type BuiltInType =
    | Arithmetic of ArithmeticType
    | Comparison of ComparisonType
    | ListF of ListFunctionType
    | IfThenElse
and ArithmeticType =
    | Add
    | Subtract
    | Multiply
    | Divide
and ComparisonType = | Eq | Ne | Lt| Gt | Le | Ge
and ListFunctionType =
    | IsList
    | IsEmpty
    | Head
    | Tail
    | ImplodeString
    | ExplodeString
    | P // in Built-in required list

// Literal types
type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    
// Abstract Syntax Tree
type Ast =
    | Lambda of Identifier * Ast
    | Call of Ast * Ast
    | FuncDefExp of Identifier * Ast * Ast
    | Combinator of CombinatorType
    | Literal of Value
    | Variable of Identifier
    | BuiltInFunc of BuiltInType
    | Pair of Ast * Ast // [1, 2, 3] = Pair(1, Pair(2, Pair(3, NULL))
    | Null
and CombinatorType =
    | I
    | S
    | K
    | Y


let rec PrintTree tree = // Recursively prints an AST tree
    match tree with
    | FuncDefExp(x, body, exp) ->
        sprintf "%A = %A in (%A)" <| x <| body <| exp
    | Call(E1, E2) ->
        sprintf "(%s %s)" <| PrintTree E1 <| PrintTree E2
    | Lambda(BV, E) ->
        sprintf "(λ%s.%s)" <| BV <| PrintTree E
    | Pair(E1, E2) ->
        let rec printList l =
            match l with
            | Null -> ""
            | Pair(E1, E2) -> sprintf "%A," E1 + printList E2
            | _ -> failwithf "shouldn't happen"
        "[" + printList (Pair(E1, E2)) + "]"
    | BuiltInFunc(IfThenElse) -> sprintf "IfThenElse"
    | Null -> "null"
    | Variable x
    | Literal(String x) -> x
    | BuiltInFunc(ListF x) -> sprintf "%A" <| x
    | BuiltInFunc(Arithmetic x) -> sprintf "%A" <| x
    | BuiltInFunc(Comparison x) -> sprintf "%A" <| x
    | Literal(Int x) -> sprintf "%A" <| x
    | Literal(Double x) -> sprintf "%A" <| x
    | Literal(Bool x) -> sprintf "%A" <| x
    | Combinator x -> sprintf "%A" <| x
    
    
module PAPHelpers = 
    let (|INT|_|) x =
        match x with
        | Literal(Int y) -> Some y
        | _ -> None

    let (|DOUBLE|_|) x =
        match x with
        | Literal(Double y) -> Some y
        | _ -> None

    let (|NUM|_|) x =
        match x with
        | Literal(Int y) -> Some <| double y
        | Literal(Double y) -> Some y
        | _ -> None

    let (|STR|_|) x =
        match x with
        | Literal(String y) -> Some y
        | _ -> None

    let (|BOOL|_|) x =
        match x with
        | Literal(Bool y) -> Some y
        | _ -> None

    let (|CONSTORVAR|_|) E =
        match E with
        | Null -> Some <| Null
        | Literal x -> Some <| Literal x
        | BuiltInFunc B -> Some <| BuiltInFunc B
        | Combinator C -> Some <| Combinator C
        | Variable x -> Some <| Variable x
        | _ -> None
        

// F# list -> list from pairs
let ListFromPairs lst =
    let rec compute l =
        match l with
        | [] -> Null
        | hd :: tail -> Pair(hd, compute tail)

    match lst with
    | [] -> Pair(Null, Null)
    | hd :: tail -> Pair(hd, compute tail)