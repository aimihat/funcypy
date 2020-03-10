open Expecto
open Lexer
open Parser
open Combinator_runtime

let expectoConfig = { defaultConfig with verbosity = Logging.LogLevel.Debug }

let Interpreter std_in =
    std_in
    |> tokeniser
    |> (pRun pExpr)
    |> Combinator_runtime



[<EntryPoint>]
let main argv =
    let testString = "2+2"
    Interpreter testString
    // runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    0