module Common

////////////////////////////////////// TOKEN DEFINITION //////////////////////////////////////
// TODO: Make sure these match with James's Token types
// TODO: implementation of something for lists
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
// TODO: This is from TINY language and needs to be changed a bit
// TODO: MAKE SURE THAT CONDITIONAL LOGIC AND BRACKETS IS PROPERLY ENCAPSULATED

// List of Start operators for recursive constructs, conditionals and functions
let startOps = [ "BEGIN"; "("; "IF" ; "FUN"] // keywords that can possibly start a recursive parse

// end of parse recursive constructs
let endOps = [ "END"; "THEN"; "ELSE"; "FI"; ")" ; "IN"] 

// List of Binary operators
let binaryOps = ["AND"; "OR"; "OR"; "OR"; "OR"]

// List of Arithmetic operators
let arithmeticOps =  ["Add"; "Subtract"; "Multiply"; "Divide"]

// List of Unary operators
let unaryOps = [ "NOT"; "-" ] // prefix unary operators

// List of Comparison operators
let comparisonOps = ["Eq"; "Ne"; "Lt"; "Gt"; "Le"; "Ge"]

////////////////////////////// COMMON CHARACTER RECOGNITION METHODS /////////////////////////////
// TODO: This is from TINY language and needs to be changed a bit

/// character is white space
let isWhiteSpace (c : char) = List.contains c [ ' '; '\n'; '\t'; '\r'; '\f' ]

/// charater is new line
let isNewLine (c : char) = List.contains c [ '\n'; '\f'; '\r' ]

/// character is alphabetic
let isAlpha (c : char) = List.contains c ([ 'a'..'z' ] @ [ 'A'..'Z' ])

/// character is a decimal digit
let isDigit (c : char) = List.contains c [ '0'..'9' ]

/// character is alphanumeic (allowed in symbol)
let isAlphaNum (c : char) = isAlpha c || isDigit c

/// character op is in list lst
let isOpInList (op: char) lst = List.contains op lst

/// characters in str match a starting prefix of x
let rec charListStartsWith (x : char list) (str : string) = 
    match x with
    | _ when str = "" -> true // empty string will match anything
    | ch :: r when str.[0] = ch -> charListStartsWith r str.[1..str.Length - 1] // if first char matches check the rest
    | _ -> false // This must be a mismatch