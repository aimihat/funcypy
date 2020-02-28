module Combinator_runtime

open Helper
open PAPHelpers

// Following L4 slides 16/17
// Bracket Abstraction implemented with the two functions below
let rec Abstract tree =
    let rec BracketAbstract var E =
        match E with
        | Variable x when var = x -> Combinator I
        | Call(E1, E2) -> Call(Call(Combinator S, BracketAbstract var E1), BracketAbstract var E2)
        | Lambda(_, _)
        | FuncDefExp(_, _, _) -> failwithf "BA001: Shouldn't happen! (%A)" E
        | other -> Call(Combinator K, other)

    match tree with
    // Call or Function
    | Call(E1, E2) -> Call(Abstract E1, Abstract E2)
    | Pair(E1, E2) -> Pair(E1, E2)
    | Lambda(v, body) -> BracketAbstract v <| Abstract body
    // neither Call nor Function
    | CONSTORVAR x -> x
    | FuncDefExp(_, _, _) -> failwithf "BA002: Shouldn't happen! (%A)" tree


// Substituting func/var definitions in final expression.
let rec InlineDefs tree =
    let rec Substitute var body E =
        match E with
        | Call(E1, E2) -> Call(Substitute var body E1, Substitute var body E2)
        | Pair(E1, E2) -> Pair(Substitute var body E1, Substitute var body E2)
        | Variable x when x = var -> body
        | CONSTORVAR x -> x
        | Lambda(v, exp) when v = var -> Lambda(v, exp)
        | Lambda(v, exp) -> Lambda(v, Substitute var body exp)
        | FuncDefExp(_, _, _) -> failwithf "ILD001: Shouldn't happen! (%A)" E

    match tree with
    | FuncDefExp(v, body, exp) -> Substitute v <| InlineDefs body <| InlineDefs exp
    | Call(E1, E2) -> Call(InlineDefs E1, InlineDefs E2)
    | Pair(E1, E2) -> Pair(InlineDefs E1, InlineDefs E2)
    | Lambda(v, body) -> Lambda(v, InlineDefs body)
    | CONSTORVAR x -> x


// Recursively evaluate AST with built-in combinators and functions
let rec Eval(tree: Ast): Ast =
    let (|BuiltinCombinator|_|) node =
        match node with
        | Call(Combinator I, E)
        | Call(Call(Combinator K, E), _) ->
            Eval E |> Some
        | Call(Combinator Y, E) ->
            Call(E, Call(Combinator Y, E))
            |> Eval |> Some
        | Call(Call(Call(Combinator S, f), g), x) ->
            Call(Call(f, x) |> Eval, Call(g, x) |> Eval)
            |> Eval |> Some
        | _ -> None

    let (|BuiltinArithm|_|) node =
        match node with
        | Call(Call(BuiltInFunc(Arithmetic op), x), y) ->
            let x', y' = Eval x, Eval y
            match x', y' with
            | INT n, INT m ->
                match op with
                | Add -> Int(n + m)
                | Subtract -> Int(n - m)
                | Divide -> Int(n / m)
                | Multiply -> Int(n * m)
            | NUM n, NUM m ->
                match op with
                | Add -> Double(n + m)
                | Subtract -> Double(n - m)
                | Divide -> Double(n / m)
                | Multiply -> Double(n * m)
            | STR s1, STR s2 when op = Add -> String(s1 + s2)
            | _ -> failwithf "Tried calling %A on %A, %A" op x' y'
            |> Literal |> Some
        | _ -> None

    let (|BuiltinIfThenElse|_|) node =
        match node with
        | Call(Call(Call(BuiltInFunc IfThenElse, cond), then_br), else_br) ->
            let cond' = Eval cond
            match cond' with
            | BOOL c when c = true -> then_br
            | NUM n when n <> 0.0 -> then_br
            | NUM _ | BOOL _ -> else_br
            | _ -> failwithf "Tried calling if statement with condition: %A" cond' 
            |> Eval |> Some
        | _ -> None

    let (|BuiltinListFuncs|_|) node =
        match node with
        | Call(Call(BuiltInFunc(ListF P), A), B) ->
            Pair(A, Pair(B, Null)) |> Eval |> Some // I'm aware this is not the same type as on website
        | Call(BuiltInFunc(ListF IsEmpty), lst) ->
            match Eval lst with
            | Pair(E1, E2) ->
                ((E1 = Null) && (E2 = Null))
                |> Bool |> Literal |> Some
            | E -> failwithf "Tried calling IsEmpty on: %A" E
        | Call(BuiltInFunc(ListF IsList), lst) ->
            match Eval lst with
            | Pair(_, _) -> Bool true
            | _ -> Bool false
            |> Literal |> Some
        | Call(BuiltInFunc(ListF Head), lst) ->
            match Eval lst with
            | Pair(E, _) when E <> Null -> Some E
            | E -> failwithf "Tried calling Head on: %A" E
        | Call(BuiltInFunc(ListF Tail), lst) ->
            match Eval lst with
            | Pair(_, E) when E <> Null -> Some E
            | E -> failwithf "Tried calling Tail on: %A" E
        | Call(BuiltInFunc(ListF ImplodeString), lst) ->
            let lst' = Eval lst
            let rec implode p = // better if implemented in the language with list reduction
                match p with
                | Pair(Literal(String s), Null) -> s
                | Pair(Literal(String s), tail) -> s + implode tail
                | _ -> failwithf "Tried calling implode on: %A" lst'
            Some <| Literal(String(implode lst'))
        | Call(BuiltInFunc(ListF ExplodeString), str) ->
            match Eval str with
            | Literal(String s) ->
                Seq.toList s
                |> List.map (fun i -> Literal (i |> string |> String))
                |> ListFromPairs |> Some
            | E -> failwithf "Tried calling explode on: %A" E
        | _ -> None

    let (|BuiltinComparison|_|) node =
        match node with
        | Call(Call(BuiltInFunc(Comparison op), x), y) ->
            let x', y' = Eval x, Eval y
            match op, x', y' with
            // = and != apply to all types.
            | Eq, n, m -> Bool(n = m)
            | Ne, n, m -> Bool(n <> m)
            // all other comparison operators apply to numerical types only
            | Lt, NUM n, NUM m -> Bool(n < m)
            | Gt, NUM n, NUM m -> Bool(n > m)
            | Le, NUM n, NUM m -> Bool(n <= m)
            | Ge, NUM n, NUM m -> Bool(n >= m)
            | _ -> failwithf "Tried calling %A on %A, %A" op x' y' // TODO: check if x'/y' fully reduced here
            |> Literal |> Some
        | _ -> None

    match tree with
    | BuiltinArithm result
    | BuiltinComparison result
    | BuiltinIfThenElse result
    | BuiltinCombinator result
    | BuiltinListFuncs result
    | CONSTORVAR result -> result
    | Pair(E1, E2) -> Pair(Eval E1, Eval E2)
    | Call(E1, E2) -> Call(Eval E1, Eval E2)
    | Lambda(_, _)
    | FuncDefExp(_, _, _) -> failwithf "EVA001: Shouldn't happen! (%A)" tree


let Interpret tree =
    tree
    |> InlineDefs // Substitutes assignments/definitions where they are used.
    |> Abstract // Abstracts all functions
    |> Eval // Combinator reduction + builtin functions


let fact =
    Call
        (Call
            (Combinator Y,
             Lambda
                 ("fact",
                  Lambda
                      ("n",
                       Call
                           (Call
                               (Call(BuiltInFunc IfThenElse, Variable "n"),
                                Call
                                    (Call(BuiltInFunc(Arithmetic Multiply), Variable "n"),
                                     Call
                                         (Variable "fact",
                                          Call(Call(BuiltInFunc(Arithmetic Subtract), Variable "n"), Literal(Int 1))))),
                            Literal(Int 1))))), Literal(Int 5))