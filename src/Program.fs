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

[<EntryPoint>]
let main argv =
    // Running tests - development

    // printfn "%A" <| pRun pAst (tokeniser "def f x = x \n f 1") // -> handled in wrapper
    // printfn "%A" <| tokeniser "" // -> handled in wrapper
    lexerTestsWithExpecto() |> ignore
    parserTestsWithExpecto() |> ignore
    endToEndTestsWithExpecto() |> ignore

    // printfn "%A" <| Interpret (pRun pAst (tokeniser "def ifFuncTest x = \n if (x<1): true \n else: false \n ifFuncTest 3"))
    printfn "%A" <| Interpret (Parse (tokeniser "def varDefinitionTest x = \n y=5 \n x+y \n varDefinitionTest 3"))

    // Running file - release
    (*
    let BuiltInCode = loadCode "src/mainlib/builtin.fpy"
    match [|"test_code.fpy"|] with 
    | [|path|] -> 
        let CombinedCode =
            try
                let UserCode = loadCode path
                Some <| BuiltInCode + "\n" + UserCode
            with
            | error -> 
                printf "File not found\n"
                None
        
        //Append built-in definitions to user code
        
        let result =
            CombinedCode
            |> Option.map tokeniser
            |> Option.map (pRun pAst)
            |> Option.map Interpret

        match result with
        | Some (Some res) -> printf "%s" <| PrintTree res
        | _ -> printf "Did not find evaluate.\n"
    | _ -> printf "Must enter a .fpy file to execute.\n"
    *)
    0