let print x = printf "%A \n" x
let printPipe x = printf "%A \n" x ; x





// Incomplete type definition, closures implemention TBD




type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = 
    | IdString of string
    | IdChar of char

type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Tuple of Value*Value

type Ast =    
    | Statement of Ast    
    | Expression of Ex    
    | Function of Identifier option * Identifier * Ast
    // | Scope of Ast list option
    | Conditional of Ex * Ast * Ast option
    | Call of Ast * Ast
    // | Assign of Identifier * Ex
    | Combinator of CombinatorType
and Ex =
    | Single of Ast
    | Literal of Value
    | Variable of Identifier
    | Arithmetic of Ex * Arithmetic * Ex
    | Comparison of Ex * Comparison * Ex
and CombinatorType = 
    | K 
    | I 
    | S

    

type LambdaTerm =
  | ExpressionT of Ex
  | LambdaT of string*LambdaTerm
  | ClosureT of string*LambdaTerm*Env
  | ApplicationT of LambdaTerm*LambdaTerm

and Env = (char*LambdaTerm) list    



let litStr x = Literal (String x)
let litInt x = Literal (Int x)
let litDouble x = Literal (Double x)

// Suppose I am give the input as "LambdaT "a" 'a'"" which is equivalent to //a.a where // means lambda
let (|MATCHARITH|_|) = 
    let innerFn inp = 
        match inp with
        | Add -> Some Add
        | Subtract -> Some Subtract
        | Multiply -> Some Multiply
        | _ -> None

    innerFn

let (|LITINT|_|) = 
    let innerFn inp = 
        match inp with 
        | Literal (Int n) -> Some n
        | _ -> None
    innerFn

let (|LITDOUBLE|_|) = 
    let innerFn inp = 
        match inp with 
        | Literal (Double n) -> Some n
        | _ -> None
    innerFn


//  This funcion verifies Arithmetic Add for Interger and Double
// Output of this funcion is Ex*Arithmetic*Ex
// Add variable and not just literals
let (|ARITHADD|_|) = 
    let someArith x = Some (Arithmetic x)
    let innerFn (inp: Ex*Arithmetic*Ex) = 
        match inp with
        | (LITINT n1, Add, LITINT n2) -> someArith  (litInt n1, Add, litInt n2)
        | (LITDOUBLE n1, Add, LITDOUBLE n2) -> someArith  (litDouble n1, Add, litDouble n2)
        | (LITINT n1, Subtract, LITINT n2) -> someArith  (litInt n1, Subtract, litInt n2)
        | (LITDOUBLE n1, Subtract, LITDOUBLE n2) -> someArith  (litDouble n1, Subtract, litDouble n2)
        | (LITINT n1, Multiply, LITINT n2) -> someArith  (litInt n1, Multiply, litInt n2)
        | (LITDOUBLE n1, Multiply, LITDOUBLE n2) -> someArith  (litDouble n1, Multiply, litDouble n2)
        | (LITINT n1, Divide, LITINT n2) -> someArith  (litInt n1, Divide, litInt n2)
        | (LITDOUBLE n1, Divide, LITDOUBLE n2) -> someArith  (litDouble n1, Divide, litDouble n2)
        | _ -> failwith "Addition Parsing Failed"
    innerFn

let (|ARITHSUBTRACT|_|) = 
    let innerFn (inp: Ex*Arithmetic*Ex) = 
        match inp with
        | (LITINT n1, Subtract, LITINT n2) -> Some  (litInt n1, Subtract, litInt n2)
        | (LITDOUBLE n1, Subtract, LITDOUBLE n2) -> Some  (litDouble n1, Subtract, litDouble n2)
        | _ -> failwith "Addition Parsing Failed"
    innerFn
    


// This funcion will outputs the correct arithmetics in the type Arithmetic(Ex*Arithmetic*Ex)
let (|ARITH|_|) = 
    let someArith x = Some (Arithmetic x)
    let innerFn inp = 
        match inp with
        | ARITHADD (exp) -> someArith exp
        | ARITHSUBTRACT (exp) -> someArith exp
        | _ -> failwith "Arithmetics Parsing Failed"

    innerFn
 

// let rec LambdaParser (inp: Ast list) = 
//   match inp with
//   | Function (None , IdString f, Expression (Variable (IdString ast))) :: rest -> LambdaT (f, listStr ast) :: LambdaParser rest
//   | Function (None , IdString f, Expression (Arithmetic (Literal n1, ARITH sign, Expression n2))) :: rest -> LambdaT (f, )
//   | _ -> failwith "Lambda Parser Failed"










////////////////// TESTS
/// 

// Test add parser