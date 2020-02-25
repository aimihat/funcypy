module Common

////////////////////////////////////// TOKEN DEFINITION //////////////////////////////////////
// TODO: Make sure these match with James's Token types
// TODO: implementation of something for lists
// TODO: Find some way of representing priority of the operators

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
type Operator = 
    | BEGIN | LRB | IF | EQUALS | FUN
    | END | THEN | ELSE | FI | RRB | IN

type UnaryOps = NOT | NEGATE
type Literal = 
    | Bool of bool 
    | Int of int 
    | Double of double 
    | String of string 
    | Tuple of Literal*Literal
type CombinatorType = | I | S | K

type Token = 
    | TokLit of Literal
    | TokUnaryOp of UnaryOps
    | TokSpecOp of Operator
    | TokIdentifier of Identifier
    | TokBuiltInOp of BuiltInType
    // | TokNull
////////////////////////////////////// OPERATOR DEFINITION //////////////////////////////////////


