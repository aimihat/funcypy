module Combinator_runtime

open System.Runtime.Intrinsics.X86
open Helpers
open PAPHelpers

let rec Substitute2 BV By In =
    // Graph search, substituting `Variable BV` by `By`
    match In with
    | Call(E1, E2, i) -> Call(Substitute2 BV By E1, Substitute2 BV By E2, i)
    | Pair(E1, E2, i) -> Pair(Substitute2 BV By E1, Substitute2 BV By E2, i)
    | Lambda(BV2, E) when BV = BV2 -> Lambda(BV2, E) // Overriden scope
    | Lambda(BV2, E) -> Lambda(BV2, Substitute2 BV By E) // Overriden scope
    | Variable x when BV = x -> By
    | E -> E

//// Bracket Abstraction implemented recursively with `Abstract` and `BracketAbstract`
let rec Abstract (node: Ast): Ast =
    let rec BracketAbstract var expr =
        match expr with
        | Variable v when v = var -> Combinator I
        | Call(e1, e2, _) -> NCall(NCall(Combinator S, BracketAbstract var e1), BracketAbstract var e2)
        | Lambda(_, _)
        | FuncDefExp(_, _, _) -> failwithf "BA001: Shouldn't happen! (%A)" expr
        | other -> NCall(Combinator K, other)

    match node with
    | Call(Lambda (bv, body), x, _) -> Substitute2 bv (Abstract x) (Abstract body)
    | Call(e1, e2, _) -> NCall(Abstract e1, Abstract e2)
    | Pair(e1, e2, _) -> NPair(Abstract e1, Abstract e2)
    | Lambda(bv, body) -> BracketAbstract bv <| Abstract body
    | CONSTORVAR x -> x
    | FuncDefExp(_, _, _) -> failwithf "BA002: Shouldn't happen! (%A)" node


// Substituting function/variable definitions where they are used.
let rec InlineDefs (tree: Ast): Ast =
    let rec Substitute var body E =
        match E with
        | Call(e1, e2, _) -> DCall(Substitute var body e1, Substitute var body e2)
        | Pair(e1, e2, _) -> DPair(Substitute var body e1, Substitute var body e2)
        | Variable v when v = var -> body
        | CONSTORVAR x -> x
        | Lambda(bv, expr) when bv = var -> Lambda(bv, expr)
        | Lambda(bv, expr) -> Lambda(bv, Substitute var body expr)
        | FuncDefExp(_, _, _) -> failwithf "ILD001: Shouldn't happen! (%A)" E

    match tree with
    | FuncDefExp(v, body, expr) ->
        RecursionMemo.TryAdd(v, body |> InlineDefs) |> ignore
        Substitute v <| InlineDefs body <| InlineDefs expr
    | Call(e1, e2, _) -> DCall(InlineDefs e1, InlineDefs e2)
    | Pair(e1, e2, _) -> DPair(InlineDefs e1, InlineDefs e2)
    | Lambda(bv, body) -> Lambda(bv, InlineDefs body)
    | CONSTORVAR x -> x


// Recursively evaluate AST with built-in combinators and functions
let rec Eval (tree: Ast): Ast =
    // Combinator reduction rules
    let (|BuiltinCombinator|_|) node =
        match node with
        | Call(Combinator I, E, _)
        | Call(Call(Combinator K, E, _), _, _) -> Some E
        | Call(Call(Combinator Y, f, _), x, _) ->
            NCall(NCall(f, NCall(Combinator Y, f)), x) |> Some
        | Call(Combinator Y, f, id) ->
            Call(Combinator Y, f, id) |> Some
        | Call(Call(Call(Combinator S, f, _), g, _), x, _) ->
            NCall(NCall(f, x), NCall(g, x)) |> Some
        | _ -> None

    // Arithmetic operations on numeric types and strings
    let (|BuiltinArithm|_|) node =
        let inline compute op n m =
            match op with
            | Add -> n + m
            | Subtract -> n - m
            | Divide -> n / m
            | Multiply -> n * m
            
        match node with
        | Call(Call(BuiltInFunc(Arithm op), x, _), y, _) ->
            let x', y' = Eval x, Eval y
            match x', y' with
            | INT n, INT m -> Int(compute op n m)
            | NUM n, NUM m -> Double(compute op n m)
            | STR s1, STR s2 when op = Add -> String(s1 + s2)
            | _ -> failwithf "Tried calling %A on %A, %A" op x' y'
            |> Literal |> Some
        | _ -> None
        
    // Comparison operations
    let (|BuiltinComparison|_|) node =
        match node with
        | Call(Call(BuiltInFunc(Comp op), x, _), y, _) ->
            let x', y' = Eval x, Eval y
            match op, x', y' with
            // = and != apply to all types.
            | Eq, n, m -> n = m
            | Ne, n, m -> n <> m
            // all other comparison operators apply to numerical types only
            | Lt, NUM n, NUM m -> n < m
            | Gt, NUM n, NUM m -> n > m
            | Le, NUM n, NUM m -> n <= m
            | Ge, NUM n, NUM m -> n >= m
            | _ -> failwithf "Tried calling %A on %A, %A" op x' y'
            |> Bool |> Literal |> Some
        | _ -> None
    
    // Lazy evaluation of conditionals
    let (|BuiltinIfThenElse|_|) node =
        match node with
        | Call(Call(Call(BuiltInFunc IfThenElse, cond, _), then_br, _), else_br, _) ->
            let cond' = Eval cond
            match cond' with
            | BOOL c when c = true -> then_br
            | NUM n when n <> 0.0 -> then_br
            | NUM _ | BOOL _ -> else_br
            | _ -> failwithf "Tried calling if statement with condition: %A" cond' 
            |> Some
        | _ -> None
    List.concat
    // Built-in functions on lists
    let (|BuiltinListFuncs|_|) node =
        match node with
        | Call(Call(BuiltInFunc(ListF P), a, _), b, _) -> // Pair
            NPair(a, NPair(b, Null)) |> Some // I'm aware this is not the same type as on website
        | Call(Call(BuiltInFunc(ListF Append), list1, _), el, _) -> // List append
            let rec appendToList lst =
                match lst with
                | Pair(e1, Null, _) -> NPair(e1, NPair(el, Null)) 
                | Pair(e1, e2, _) -> NPair(e1, appendToList e2)
                | _ -> failwithf "Tried calling Append on %A" list1
            appendToList list1 |> Some
        | Call(BuiltInFunc(ListF IsEmpty), lst, _) -> // IsEmpty
            match Eval lst with
            | Pair(Null, Null, _) -> true
            | Pair(_, _, _) -> false
            | e -> failwithf "Tried calling IsEmpty on: %A" e
            |> Bool |> Literal |> Some
        | Call(BuiltInFunc(ListF IsList), lst, _) -> // IsList
            match Eval lst with
            | Pair(_, _, _) -> true
            | _ -> false
            |> Bool |> Literal |> Some
        | Call(BuiltInFunc(ListF Head), lst, _) -> // Head
            match Eval lst with
            | Pair(e, _, _) when e <> Null -> Some e
            | e -> failwithf "Tried calling Head on: %A" e
        | Call(BuiltInFunc(ListF Tail), lst, _) -> // Tail
            match Eval lst with
            | Pair(_, e, _) when e <> Null -> Some e
            | e -> failwithf "Tried calling Tail on: %A" e
        | Call(BuiltInFunc(ListF ImplodeStr), lst, _) -> // ImplodeStr
            let rec Implode p = // better if implemented in the language with list reduction
                match p with
                | Pair(Literal(String s), tail, _) -> s + Implode tail
                | Null -> ""
                | _ -> failwithf "Tried calling implode on: %A" <| Eval lst 
            lst |> Eval |> Implode |> String |> Literal |> Some
        | Call(BuiltInFunc(ListF ExplodeStr), str, _) -> // ExplodeStr
            match Eval str with
            | Literal(String s) ->
                Seq.toList s
                |> List.map (fun i -> Literal (i |> string |> String))
                |> ListFromPairs |> Some
            | e -> failwithf "Tried calling explode on: %A" e
        | _ -> None

    let Reduce node =
        let EvalIfChanged e1 e2 id f =
            // If `Call` or `Pair` arguments change after evaluation
            // they may now match a built-in, otherwise cannot reduce further
            let e1', e2' = Eval e1, Eval e2
            if (e1' = e1) && (e2' = e2)
            then f(e1', e2', id)
            else Eval <| f(e1', e2', id)
        
        match node with
        | BuiltinArithm result
        | BuiltinComparison result
        | BuiltinIfThenElse result
        | BuiltinCombinator result
        | BuiltinListFuncs result -> Eval result
        | Call(Variable f, x, i) ->
            match RecursionMemo.TryGetValue f with
            | true, result -> NCall(result, x) |> Abstract |> Eval
            | false, _ -> Call(Variable f, x, i)
        | CONSTORVAR result -> result
        | Pair(e1, e2, id) -> EvalIfChanged e1 e2 id Pair
        | Call(e1, e2, id) -> EvalIfChanged e1 e2 id Call
        | Lambda(_, _)
        | FuncDefExp(_, _, _) -> failwithf "EVA001: Shouldn't happen! (%A)" node
        
    match tree with
    | Call(e1, e2, _)
    | Pair(e1, e2, _) ->
        let IDs = getID e1, getID e2
        GetMemoOrAdd IDs Reduce tree
    | _ -> 
        Reduce tree

let Interpret tree =
    match tree with
    | Some node ->
        node
        |> fst
        |> InlineDefs // Substitutes assignments/definitions where they are used.
        |> Abstract // Abstracts all functions
        |> Eval // Combinator reduction + builtin functions
        |> Some
    | None -> None
    