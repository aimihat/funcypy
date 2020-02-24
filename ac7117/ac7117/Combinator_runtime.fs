module Combinator_runtime

open System
open System.Linq.Expressions
open Helper

// Following L4 slides 16/17
// Bracket Abstraction implemented with the two functions below
let rec Abstract tree =
    let rec BracketAbstract var E =
        match E with
        | Expression (Variable x) when var = x
            -> Combinator I
        | Call(E1, E2)
            -> Call(Call(Combinator S, BracketAbstract var E1), BracketAbstract var E2)
        | Combinator C -> Call(Combinator K, Combinator C)
        | Expression E -> Call(Combinator K, Expression E)
        | Function(_, _, _) -> failwithf "Should not happen!" 

    match tree with
    // Call or Function
    | Call(E1, E2) -> Call(Abstract E1, Abstract E2)
    | Function(Name, BV, Body) ->
        BracketAbstract BV <| Abstract Body
    // neither Call nor Function
    | Expression(Single A) -> Abstract A
    | Combinator C -> Combinator C
    | Expression (Literal x) -> Expression (Literal x)
    | Expression (Variable x) -> Expression (Variable x)
    | Expression (BuiltInFunc x) -> Expression (BuiltInFunc x)


// Generic PAP
let (|INT|_|) x =
    match x with
    | Expression(Literal(Int y)) -> Some y
    | _ -> None 
        
//Repeat in a tail recusrive function until at top level the function has not enough arguments for its reduction rule, or until you have just a literal.
let rec Eval tree =
    let (|BuiltinCombinator|_|) node =
        match node with
        | Call(Combinator I, E)
        | Call(Call(Combinator K, E), _) ->
            Eval E |> Some
        | Call(Call(Call(Combinator S, f), g), x) ->
            let x' = Eval x
            Call(Call(Eval f, x') |> Eval, Call(Eval g, x')|> Eval)
            |> Eval |> Some
        | _ -> None
        
    let (|BuiltinArithm|_|) node =
        match node with
        | Call(Call(Expression(BuiltInFunc (Arithmetic op)), x), y) ->
            let x' = Eval x
            let y' = Eval y
            match op, x', y' with
            | Add, INT n, INT m -> Expression(Literal <| Int (n + m))
            | Subtract, INT n, INT m -> Expression(Literal <| Int (n - m))
            | Divide, INT n, INT m -> Expression(Literal <| Int (n / m))
            | Multiply, INT n, INT m -> Expression(Literal <| Int (n * m))
            | _ -> failwithf "Tried calling %A on %A, %A" op x' y' // TODO: check if x'/y' fully reduced here
            |> Some
        | _ -> None
        
    let (|BuiltinComparison|_|) node =
        match node with
        | Call(Call(Expression(BuiltInFunc (Comparison op)), x), y) ->
            let x' = Eval x
            let y' = Eval y
            match op, x', y' with
            // = and != apply to all types.
            | Eq, n, m -> Expression(Literal <| Bool (n = m))
            | Ne, n, m -> Expression(Literal <| Bool (n <> m))
            // all other comparison operators apply to numerical types only
            | Lt, INT n, INT m -> Expression(Literal <| Bool (n < m))
            | Gt, INT n, INT m -> Expression(Literal <| Bool (n > m))
            | Le, INT n, INT m -> Expression(Literal <| Bool (n <= m))
            | Ge, INT n, INT m -> Expression(Literal <| Bool (n >= m))
            | _ -> failwithf "Tried calling %A on %A, %A" op x' y' // TODO: check if x'/y' fully reduced here
            |> Some
        | _ -> None
    
    match tree with
    | BuiltinArithm result
    | BuiltinComparison result
    | BuiltinCombinator result -> result
    | Expression(Single A) -> Expression(Single (Eval A))
    | Expression(Variable x) -> Expression(Variable x) //TODO: env
    | Expression(BuiltInFunc F) -> Expression(BuiltInFunc F) 
    | Combinator C -> Combinator C
    | Expression(Literal x) -> Expression(Literal x)
    | Call(E1, E2) ->
        Call(Eval E1, Eval E2) //return call with eval params, since no builtin can process it
    | Function(_,_,_) -> failwithf "Shouldn't happen?"

let Interpret tree =
    tree
    |> Abstract
    |> Eval