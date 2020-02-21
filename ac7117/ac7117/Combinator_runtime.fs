module Combinator_runtime

//open Helper

type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Tuple of Value * Value
    | List of Value * Value

type Identifier = string

type Ast =
    | Lambda of Identifier * Ast
    | FuncApp of Ast * Ast
    | Variable of Identifier
    | Combinator of CombinatorType

and CombinatorType =
    | K
    | I
    | S

// All you do is repeatedly evaluate combinators & built-in operators according to rules,
// till you end up with a result you can't evaluate further

let rec Abstract (Tree: Ast) (v: Ast): Ast = // Abstract v from Tree
    // This assumes curried application, might need to convert from Yannis AST
    match Tree with
    | FuncApp(E1, E2) -> FuncApp(FuncApp(Combinator S, Abstract E1 v), Abstract E2 v)
    | Lambda(BV, E) when Variable BV = v -> Abstract E v
    | Lambda(BV, E) -> Lambda(BV, Abstract E v)
    | Combinator x -> FuncApp(Combinator K, Combinator x) // clarify if this and below is right
    | E when E = v -> Combinator I
    | E -> E

let AbstractArgs (Tree: Ast) (args: Ast list) =
    List.fold (fun t arg -> Abstract t arg) Tree args
    

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

let CombFunction x =
    match x with
    | K -> (fun x y -> x)
    | I -> (fun x -> x)
    | S -> (fun f g x -> (f x) (g x))

let rec Substitute BV By In =
    // Graph search, substituting `Variable BV` by `By`
    match In with
    | FuncApp(E1, E2) -> FuncApp(Substitute BV By E1, Substitute BV By E2)
    | Lambda(BV2, E) when BV = BV2 -> Lambda(BV2, E) // Overriden scope
    | Lambda(BV2, E) -> Lambda(BV2, Substitute BV By E) // Overriden scope
    | Variable x when BV = x -> By
    | E -> E

let rec EvalTree (Node: Ast) = // β-reduction
    match Node with
    | Lambda (BV, E) -> Lambda (BV, EvalTree E)
    | FuncApp (Lambda (BV, E1), E2) -> EvalTree (Substitute BV E2 E1) // how to handle built-in operators
    | E -> E //matches built-in operators and literals
    
let rec printCombExpr Tree =
    match Tree with
    | FuncApp(E1, E2) -> sprintf "(%s %s)" <| printCombExpr E1 <| printCombExpr E2
    | Lambda(BV, E) -> sprintf "(λ%s.%s)" <| BV <| printCombExpr E
    | Combinator x -> sprintf "%A" <| x
    | Variable x -> x
    
let InputAst = FuncApp(Lambda("g", Combinator I), Variable "f")
let AbstractVars = [Variable "g"; Variable "f"]
sprintf "%A" <| EvalTree (FuncApp(Lambda("x", Variable "x"), Variable "y"))

    

let Interpreter(Tree: Ast): Result<'a, string> =
    Tree
    //|> ParseAST // Ensure Ast can be turned to combinators
    |> Abstract // Bracket abstraction: turn AST into a constant expression using combinators
    //|> EvalCombinators // β-reduction: combinator reduction/evaluation
    |> Ok
