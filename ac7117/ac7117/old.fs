module old

open System
open System.Linq.Expressions

// All you do is repeatedly evaluate combinators & built-in operators according to rules,
// till you end up with a result you can't evaluate further


// TODO: think how to handle assignments in ast
// TODO: add more combinators, for better efficiency (hopefully, have time)
// TODO: recursive
// TODO: memoise

(*TODO:
Q10. The makeHeap function reads an expression into heap.
It accepts an environment which contains all previously
(outer in the parse tree) defined symbols and their values as parse expressions.
When the expression is read names are replaced by the corresponding values.
The initial environment is the set of built-in functions - operators and combinators.
What happens when a recursive function (like fpp above) is loaded into memory?

Q11. (E) Using the Y fixed point combinator defined by the reduction rule Y f = f (Y f)
and bracket abstracting fpp from the body of fpp is another (cleverer) way to handle recursion.
Give a definition of fpp without recursion using the Y combinator.
Show that it works by hand reduction of one loop. Prove that it works.


*)
//The usual (and best) order is Normal order where the function (left) side of a function application
//is evaluated as much as possible before the function parameters

(*
let rec Substitute (bv: Identifier) (by: Ast) (expr: Ast) =
    // Graph search, substituting `Variable BV` by `By`
    match expr with
    | Call(E1, E2) -> Call(Substitute bv by E1, Substitute bv by E2)
    | Function(name, arg, E) when bv = arg -> Function(name, arg, E) // Overriden scope
    | Function(name, arg, E) -> Function(name, arg, Substitute bv by E) // Overriden scope
    | Expression(Variable x) when bv = x -> by
    //TODO: | Conditional(Variable x, br_then, Some br_else) -> Conditional(by, br_then, Some br_else)
    //TODO: handle other Ast cases
    | E -> E



let rec Abstract (tree: Ast) (v: Identifier) inlambda: Ast = // todo: remove inlambda
    match tree with
    | Call(Call(Combinator S, E1), E2) -> Call(Call(Combinator S, Abstract E1 v inlambda), Abstract E2 v inlambda)
    | Call(Combinator K, E) -> Call(Combinator K, Abstract E v inlambda)
    | Combinator I when inlambda = true -> Call(Combinator K, Combinator I)
    | Call(E1, E2) -> Call(Call(Combinator S, Abstract E1 v inlambda), Abstract E2 v inlambda)
    | Function(None, BV, E) when BV = v -> Abstract E v true
    | Function(None, BV, E) -> Function(None, BV, Abstract E v inlambda)
    | Expression(Variable E) when E = v -> Combinator I
    | Expression(Variable E) -> Call(Combinator K, Expression(Variable E))
    | E -> E

let AbstractArgs (tree: Ast) (args: Identifier list) =
    List.fold (fun t arg -> Abstract t arg false) tree args

let (|MatchLIT|_|) lit =
    match lit with
    | Expression(Literal x) -> Some <| Expression(Literal x)
    | _ -> None

let (|GetINT|_|) lit =
    match lit with
    | Expression(Literal(Int x)) -> Some x
    | _ -> None

let (|GetBOOL|_|) lit =
    match lit with
    | Expression(Literal(Bool x)) -> Some x
    | _ -> None

let rec EvalTree(node: Ast) = // β-reduction
    
    let EvalExpr(expr: Ex) = EvalTree(Expression expr)
        
    let (|ArithmeticOp|_|) node =
        let opMapAri =
                Map
                    [ Add, (+)
                      Subtract, (-)
                      Divide, (/)
                      Multiply, (*)
                      ]
                    
        match node with
        | Expression(Arithmetic(expr1, op, expr2)) ->
            let E1, E2 = EvalExpr expr1, EvalExpr expr2
            match (E1, E2) with
            | MatchLIT i1, MatchLIT i2 ->
                // both operands are literals. therefore, either successfully evaluates or types are invalid
                match (i1, i2) with
                | GetINT i1, GetINT i2 ->
                    Some <| Expression(Literal(Int( Option.get (opMapAri.TryFind op) i1 i2))) // if both are literals, evaluate
                | _ -> failwithf "Calling operator %A on %A, %A" op i1 i2
            | _ ->
                Some <| Expression(Arithmetic(Single E1, op, Single E2)) // otherwise, return maxim. reduced ast
        | _ -> None

    let (|ComparisonOp|_|) node =
        // Template for arithmetic and comparison PAPs
        let opMapComp =
                Map
                    [ Eq, ((=), true)
                      Ne, ((<>), true)
                      Lt, ((<), false)
                      Gt, ((>), false)
                      Le, ((<=), false)
                      Ge, ((>=), false) ]
                    
        match node with
        | Expression(Comparison(expr1, op, expr2)) ->
            let E1, E2 = EvalExpr expr1, EvalExpr expr2
            let op_fn, all_types = Option.get (opMapComp.TryFind op)
            match (E1, E2) with
            | MatchLIT i1, MatchLIT i2 ->
                // both operands are literals. therefore, either successfully evaluates or types are invalid
                match (i1, i2) with
                | GetINT i1, GetINT i2 ->
                    Some <| Expression(Literal(Bool(op_fn i1 i2))) // if both are literals, evaluate
                | GetBOOL i1, GetBOOL i2 when all_types = true ->
                    Some <| Expression(Literal(Bool((>) i1 i2))) // if both are literals, evaluate
                | _ -> failwithf "Calling operator %A on %A, %A" op i1 i2
            | _ ->
                Some <| Expression(Comparison(Single E1, op, Single E2)) // otherwise, return maxim. reduced ast
        | _ -> None

    let (|CombinatorOp|_|) node =
        // Combinator reduction rules
        match node with
        | Call(Combinator I, exp)
        | Call(Call(Combinator K, exp), _) -> Some <| EvalTree exp
        | Call(Call(Call(Combinator S, f), g), x) ->
            Some <| Call(Call(EvalTree f, EvalTree x), Call(EvalTree g, EvalTree x))
        | _ -> None
   
    //TODO: name -> lookup value
    printf "%A\n" node
    match node with
    | Function(name, bv, expr) -> Function(name, bv, EvalTree (Abstract expr bv true))
    | ComparisonOp result | ArithmeticOp result | CombinatorOp result -> result
    | Call(exp1, exp2) -> Call(EvalTree exp1, EvalTree exp2)
    | Combinator x -> Combinator x
    | Conditional(exp, br_then, br_else) ->
        let condition = EvalTree(Expression exp)
        match condition with
        | Expression(Literal(Bool true)) -> EvalTree br_then
        | Expression(Literal(Bool false)) -> EvalTree br_else
        | E -> Conditional(Single E, EvalTree br_then, EvalTree br_else)
    | Expression(Single x) -> EvalTree x
    | Expression(Arithmetic(_, _, _))
    | Expression(Comparison(_, _, _)) -> failwithf "Value type not implemented?"
    | Expression(Literal x) -> Expression(Literal x)
    | Expression(Variable x) -> Expression(Variable x) //TODO: see on left

let Interpret(tree: Ast): Result<Ast, string> =
    tree
    |> EvalTree // TODO: implement error strings? if any (could be parser only)
    |> Ok
    
(*
TODO order:
ensure working abstraction with tests
environment/assignments
recursion
memoise
*)

let InputAst = Call(Function(None, "f", Expression(Arithmetic(Variable "f", Add, Literal (Int 2)))), Expression(Literal (Int 2))) //(λf.f+2) 2
//let expected = Ok <| Expression(Literal(Int 4))
(Interpret <| InputAst)
*)