module Common

////////////////////////////////////// TOKEN DEFINITION //////////////////////////////////////
// TODO: Make sure these match with James's Token types
// TODO: implementation of something for lists
// TODO: Find some way of representing priority of the operators

type Arithmetic = ADD | SUBTRACT | MULTIPLY | DIVIDE
type Comparison = EQ | NE | LT | GT | LE | GE
type BuiltInType = Arithmetic of Arithmetic

type Identifier = string
type StartOps = BEGIN | LRB | IF | FUN | EQUALS
type EndOps = END | THEN | ELSE | FI | RRB | IN
type BinaryOps = AND | OR | NOR | XOR
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
    | TokBinOp of BinaryOps
    | TokStartOp of StartOps
    | TokEndOp of EndOps
    | TokIdentifier of Identifier
    | TokComparisonOp of Comparison
    | TokArithmeticOp of Arithmetic
    // | TokNull
////////////////////////////////////// OPERATOR DEFINITION //////////////////////////////////////


