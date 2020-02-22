module Common

////////////////////////////////////// TOKEN DEFINITION //////////////////////////////////////
// TODO: Make sure these match with James's Token types
type Token = 
    | TokIdentifier of string // identifier token to contain alphanumeric name: Func1
    // Literal Tokens
    | TokBoolLit of bool
    | TokIntLit of int // integer literal 1234 (only allow positive literals but unary - can be used to make them negative)
    | TokFloatLit of float
    | TokStrLit of string
    // Operator Tokens: like arithmetic operators, binary operators, comparator operators
    // Note: could also be keywords like "LET" and "IF"
    | TokSpec of string
    | TokNull // representation of a Null value used for empty lists: a :: b :: c :: Null (like [] in F#)

////////////////////////////////////// OPERATOR DEFINITION //////////////////////////////////////
// TODO: Find some way of representing priority of the operators

// List of Binary operators
let binaryOps = ["AND"; "OR"; "OR"; "OR"; "OR"]

// List of Arithmetic operators
let arithmeticOps =  ["Add"; "Subtract"; "Multiply"; "Divide"]

// List of Unary operators
let unaryOps = [ "NOT"; "-" ] // prefix unary operators

// List of Comparison operators
let comparisonOps = ["Eq"; "Ne"; "Lt"; "Gt"; "Le"; "Ge"]