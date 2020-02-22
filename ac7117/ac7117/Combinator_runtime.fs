module Combinator_runtime

open System
open System.Linq.Expressions
open Helper

// All you do is repeatedly evaluate combinators & built-in operators according to rules,
// till you end up with a result you can't evaluate further

let rec printTree tree =
    // Recursively prints an AST tree
    match tree with
    | Call(E1, E2) -> sprintf "(%s %s)" <| printTree E1 <| printTree E2
    | Function(None, BV, E) -> sprintf "(λ%s.%s)" <| BV <| printTree E
    | Function(Some name, BV, E) ->
        sprintf "(%s:%s.%s)" <| name <| BV <| printTree E
    | Expression(Variable x) -> x
    | Expression(Arithmetic(E1, op, E2)) ->
        sprintf "(%A %s %s)" <| op <| printTree (Expression E1) <| printTree (Expression E2)
    | Expression(Literal x) -> sprintf "%A" <| x
    | Combinator x -> sprintf "%A" <| x
    | Expression(Single x) -> printTree x

let rec Abstract (tree: Ast) (v: Identifier): Ast = // Abstract v from Tree
    // This assumes curried application, might need to convert from Yannis AST
    match tree with
    | Call(E1, E2) -> Call(Call(Combinator S, Abstract E1 v), Abstract E2 v)
    | Function(None, BV, E) when BV = v -> Abstract E v
    | Function(None, BV, E) -> Function(None, BV, Abstract E v)
    | Combinator x -> Call(Combinator K, Combinator x) // clarify if this and below is right
    | Expression (Variable E) when E = v -> Combinator I
    | E -> E

let AbstractArgs (tree: Ast) (args: Identifier list) =
    List.fold (fun t arg -> Abstract t arg) tree args
    
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
*)
    
let (|GetINT|_|) lit =
    match lit with
    | Expression (Literal (Int x)) -> Some x
    | _ -> None

let rec EvalTree (node: Ast) = // β-reduction
    let EvalExpr (expr: Ex) = EvalTree (Expression expr)
   
    let (|ArithmeticOp|_|) node =
        let (|TemplateOp|_|) op op_fn node =
            match node with
            | Expression (Arithmetic (expr1, op, expr2)) ->
                let E1, E2 = EvalExpr expr1, EvalExpr expr2
                match (E1, E2) with 
                | GetINT i1, GetINT i2
                    -> Some <| Expression (Literal (Int (op_fn i1 i2))) // if both are literals, evaluate
                | _ -> Some <| Expression (Arithmetic (Single E1, op, Single E2)) // otherwise, return maxim. reduced ast
            | _ -> None
        
        let (|Addition|_|) = (|TemplateOp|_|) Add (+)
        let (|Subtraction|_|) = (|TemplateOp|_|) Subtract (-)
        let (|Multiplication|_|) = (|TemplateOp|_|) Multiply (*)
        let (|Division|_|) = (|TemplateOp|_|) Divide (/)
        
        match node with
        | Addition result -> Some result
        | Subtraction result -> Some result
        | Multiplication result -> Some result
        | Division result -> Some result
        | _ -> None
        
    let (|CombinatorOp|_|) node =
         match node with
            | Combinator X ->
                match X with
                | I -> Some <| Function(None, "a", Expression(Variable "a"))
                | K -> Some <| Function(None, "a", Expression(Variable "a"))
                | S -> Some <| Function(None, "a", Expression(Variable "a"))
            | _ -> None
                
    match node with
    | Function(name, bv, expr) -> Function(name, bv, EvalTree expr)
    | Call(expr1, expr2) -> EvalTree (Substitute bv expr2 expr1)
    //TODO: handle call with identifier instead of actual function (e.g. function previously assigned)
    | ArithmeticOp result | CombinatorOp result -> result
    //TODO: add missing + all Exp
    //| E -> E //matches built-in operators and literals
    
let InputAst = Call(Function(None, "g", Combinator I), Expression (Variable "f"))
let AbstractVars = ["g"; "f"]

printTree <| EvalTree (AbstractArgs InputAst AbstractVars)    

let Interpreter(Tree: Ast): Result<'a, string> =
    Tree
    //|> ParseAST // Ensure Ast can be turned to combinators
    |> Abstract // Bracket abstraction: turn AST into a constant expression using combinators
    //|> EvalCombinators // β-reduction: combinator reduction/evaluation
    |> Ok
