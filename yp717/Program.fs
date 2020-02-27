open System
// open Tokeniser
open Common
open Parser 
open Testing

// mock parser example from Don Syme
let src2 = "2+5"
let print x = printfn "%A" x

[<EntryPoint>]
let main(argv) =     

    // let tokenInput2 = [TokSpecOp LAMBDA; TokIdentifier (IDString "f"); TokSpecOp ARROWFUNC; TokLit (Int 3); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokIdentifier (IDString "f")]
    // pRun pExpr tokenInput2 |> printf "%A"        
    parserTestsWithExpecto() |> ignore

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
  
    // lambda f -> 3*(f + 10)
    // lambda f -> (*) 3 (f + 10)
    // lambda f -> (((*) 3) (((+) f) 10))
    // lambda f -> * 3 ((+) f 10)

