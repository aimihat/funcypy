module Helper

type ArithmeticType = Add | Subtract | Multiply | Divide
type ComparisonType = Eq | Ne | Lt | Gt | Le | Ge
type BuiltInType = | Arithmetic of ArithmeticType | Comparison of ComparisonType

type Identifier = string

type Value =
    | Bool of bool
    | Int of int
    //| Double of double
    //| String of string
    //| Tuple of Value * Value
    //| List of Value * Value

type Ast =    
    //| Statement of Ast
    | Expression of Ex
    | Function of Identifier option * Identifier * Ast
    //| Scope of Ast list option
    //| Conditional of Ex * Ast * Ast //TODO: maybe just implement with church boolean
    | Call of Ast * Ast
    //| Assign of Identifier * Ex
    | Combinator of CombinatorType
and Ex =
    | Single of Ast
    | Literal of Value
    | Variable of Identifier
    | BuiltInFunc of BuiltInType
and CombinatorType = | I | S | K

let rec printTree tree = // Recursively prints an AST tree
    match tree with
    | Call(E1, E2) ->
        sprintf "(%s %s)" <| printTree E1 <| printTree E2
    | Function(None, BV, E) ->
        sprintf "(λ%s.%s)" <| BV <| printTree E
    | Function(Some name, BV, E) ->
        sprintf "(%s:%s.%s)" <| name <| BV <| printTree E
    | Expression(Variable x) ->
        x
    | Expression(Literal(Int x)) ->
        sprintf "%A" <| x
    | Expression(Literal(Bool x)) ->
        sprintf "%A" <| x
    | Expression(Single x) ->
        printTree x
    | Combinator x ->
        sprintf "%A" <| x
    | Expression(BuiltInFunc (Arithmetic x)) ->
        sprintf "%A" <| x
    | Expression(BuiltInFunc (Comparison x)) ->
        sprintf "%A" <| x 