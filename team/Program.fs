open Expecto
open Lexer
open Parser
open Combinator_runtime
open Helpers

let expectoConfig = { defaultConfig with verbosity = Logging.LogLevel.Debug }

//let Interpreter std_in =
//    std_in
//    |> tokeniser
//    |> (pRun pExpr)
//    |> Interpret

let x =
    FuncDefExp("f",
               Lambda("y",
                      FuncDefExp("x", Literal (Int 2), DCall(DCall(BuiltInFunc (Arithm Multiply), Variable "x"), Variable "y")))
               , DCall(Variable "f", Literal (Int 3)))


[<EntryPoint>]
let main argv =
    let testString = "2+2"
    let tokenTest1 = [TokSpecOp DEF ; TokIdentifier ("f") ; TokIdentifier ("y") ; TokSpecOp EQUALS ; TokWhitespace LineFeed ; TokSpecOp DEF ; TokIdentifier ("x") ; TokSpecOp EQUALS ; TokLit (Int 3) ; TokWhitespace LineFeed ; TokIdentifier ("x") ; TokBuiltInOp (Arithm Multiply) ; TokIdentifier ("y") ; TokWhitespace LineFeed ; TokIdentifier ("f") ; TokLit (Int 2)]
    printf "%A" <| (pRun pExpr tokenTest1)
    // runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    System.Console.ReadKey() |> ignore
    0