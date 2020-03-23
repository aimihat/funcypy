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
    
    

[<EntryPoint>]
let main argv =
//    printf "%A\n" RecursionMemo
//    printf "%A\n" ("def factorial x =\n if (x==0) then 1 else (x*(factorial (x-1)))\n factorial 4" |> tokeniser |> (pRun pAst) |> Interpret)
//    printf "%A\n" (testCase1 |> tokeniser |> (pRun pAst) |> Interpret)
//    printf "%A\n" RecursionMemo
    // Running tests - development

    // Running file - release
 
    let BuiltInCode = loadCode "src/mainlib/builtin.fpy"
    match [|"test_code.fpy"|] with 
    | [|path|] -> 
        let CombinedCode =
            try
                let UserCode = loadCode path
                Some <| BuiltInCode + "\n" + UserCode
//                Some <| UserCode    
            with
            | error -> 
                printf "File not found\n"
                None
        
        //Append built-in definitions to user code
        match CombinedCode with
        | Some code ->
            printf "%A\n" code
            let result = code |> tokeniser
            printf "%A" result 
        | _ -> printf "No code"
//        let result =
//            CombinedCode
//            |> Option.map tokeniser
//            |> Option.map (pRun pAst)
//            |> Option.map Interpret

//        match result with
//        | Some (Some res) -> printf "%s" <| PrintTree Null
//        | _ -> printf "Did not find evaluate.\n"
    | _ -> printf "Must enter a .fpy file to execute.\n"
    
    0