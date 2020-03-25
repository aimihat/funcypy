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

let inline printChain i =
    printf "%A\n------------------\n" i
    i

let testCase =
    "def g x =\n2*x\ndef f x =\ng x\nf 2"

let testCase1 =
    "def factorial x =\n if (x==0) then 1 else (x*(factorial (x-1)))\n factorial 4"
    
let RUN_TESTS = true;

[<EntryPoint>]
let main argv =
    // Running tests - development
    if RUN_TESTS
    then
        lexerTestsWithExpecto() |> ignore
        parserTestsWithExpecto() |> ignore
        endToEndTestsWithExpecto() |> ignore
        
    // Running file - release
    // let BuiltInCode = loadCode "src/mainlib/builtin.fpy"
    // match argv with 
    // | [|path|] ->
    //     let UserCode = loadCode path
    //     let CombinedCode =
    //         try
    //             Some <| BuiltInCode + "\n" + UserCode
    //         with
    //         | error -> 
    //             printf "File not found\n"
    //             None
        
    //     //Append built-in definitions to user code
    //     match CombinedCode with
    //     | Some code ->
    //         let CodeNoComments = code |> removeComments
    //         printf "\n\"\"\"\n%s\n\"\"\"\n" UserCode
    //         let result = CodeNoComments |> Tokenise |> Parse |> Interpret
    //         let prettyOutput = result |> Option.map PrintTree
    //         match prettyOutput with
    //         | Some out -> printf "-----------------\n%s\n" out
    //         | _ -> printf "No output"
    //     | _ -> printf "No code"
    // | _ -> printf "Must enter a .fpy file to execute.\n"

    0