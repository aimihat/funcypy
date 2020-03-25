open Expecto
open EndToEndTests
open ParserTests
open TestsLexer
open Lexer
open Parser
open Combinator_runtime
open Helpers
open System.IO

let expectoConfig = { defaultConfig with verbosity = Logging.LogLevel.Debug }

let RUN_TESTS = false;

[<EntryPoint>]
let main argv =
    // Running tests - development
    if RUN_TESTS
    then
        lexerTestsWithExpecto() |> ignore
        parserTestsWithExpecto() |> ignore
        endToEndTestsWithExpecto() |> ignore
        
    // Running file - release
    let BuiltInCode = loadCode "src/mainlib/builtin.fpy"
    match argv with 
    | [|path|] ->
        let UserCode = loadCode path
        let CombinedCode =
            try
                Some <| BuiltInCode + "\n" + UserCode
            with
            | error -> 
                printf "File not found\n"
                None
        
        //Append built-in definitions to user code
        match CombinedCode with
        | Some code ->
            let CodeNoComments = code |> removeComments
            printf "\n\"\"\"\n%s\n\"\"\"\n" UserCode
            let result = CodeNoComments |> Tokenise |> Parse |> Interpret
            let prettyOutput = result |> Option.map PrintTree
            match prettyOutput with
            | Some out -> printf "-----------------\n%s\n" out
            | _ -> printf "No output"
        | _ -> printf "No code"
    | _ -> printf "Must enter a .fpy file to execute.\n"

    0