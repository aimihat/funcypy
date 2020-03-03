module Common

////////////////////////////////////// TOKEN DEFINITION //////////////////////////////////////
type BuiltInType = 
    | ADD 
    | SUBTRACT 
    | MULTIPLY 
    | DIVIDE
    | EQ 
    | NE 
    | LT 
    | GT 
    | LE 
    | GE

type Identifier = IDString of string
 
/// Inclusion of FI, BEGIN, END in language grammar could make parsing multiple expressions easier
type Operator = 
    | LRB 
    | IF 
    | EQUALS 
    | LET 
    | LETREC
    | THEN
    | ELSE
    | RRB 
    | LAMBDA 
    | ARROWFUNC

/// Unary operators: implementation pending group langauge grammar review - can easily be added
/// in with C.E. 
type UnaryOps = NOT | NEGATE

/// Literal Type definitions; currently tuples can only contain two literals but not expressions
/// this can be a point for further development in the group phase
type Literal = 
    | Bool of bool 
    | Int of int 
    | Double of double 
    | String of string 
    | Tuple of Literal*Literal

/// CombinatorType included for combinator runtime
type CombinatorType = 
    | I
    | S
    | K

/// These types may potentially be useful but could be removed; depends on multiple expression
/// implementation
type Whitespace = 
    | Space             // ' '
    | FormFeed          // '\f'
    | LineFeed          // '\n'
    | CarriageReturn    // '\r'
    | HorizontalTab     // '\t'
    | VerticalTab       // '\v'

type Token = 
    | TokLit of Literal
    | TokUnaryOp of UnaryOps
    | TokSpecOp of Operator
    | TokIdentifier of Identifier
    | TokBuiltInOp of BuiltInType
    | TokWhitespace of Whitespace

//////////////////////////////////////// AST DEFINITION ///////////////////////////////////////

type Ast =
    | Const of Literal
    | Var of Identifier
    | FuncApp of Ast * Ast
    | BuiltInFunc of BuiltInType
    | FuncDefExp of Identifier * list<Ast> * Ast 
    | FuncDefExpRec of Identifier * list<Ast> * Ast
    | Lambda of Ast * Ast
    | Conditional of Ast * Ast * Ast