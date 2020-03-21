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
    let wrapper input:string = 
        let tokeniseResult = tokeniser input
        let numTokens = tokeniseResult.Length
        let parseResult = pRun pAst tokeniseResult
        match parseResult with
            | Some(tree, index) ->
                if index = numTokens then 
                    sprintf "%A" <| Interpret(parseResult)                    
                else 
                    failwithf "Failed Parse: Not all tokens parsed. Check bracket pairs."    
            | None -> failwithf "Failed Parse: Check bracket pairs and Function Definitions"

    // Uneven number of brackets around value 1
    // printfn "%A" <| pRun pAst [TokSpecOp LRB ; TokLit (Int 5) ; TokSpecOp RRB ; TokSpecOp RRB] // not throwing exception so whats up
    
    // Bad Function Definition without new line
    // printfn "%A" <| wrapper "def x = 2"
    
    // [TokSpecOp DEF; TokIdentifier "x"; TokSpecOp EQUALS; TokLit (Int 2)]

    // printfn "%A" <| wrapper "((5)" // -> handled in wrapper
    // printfn "%A" <| tokeniser "" // -> handled in wrapper
    lexerTestsWithExpecto() |> ignore
    parserTestsWithExpecto() |> ignore
    endToEndTestsWithExpecto() |> ignore

    // printfn "%A" <| wrapper "def x 2" // returns null
    // printfn "%A" <| wrapper "" // returns null
    // printfn "%A" <| wrapper "(2))" // returns null
    // printfn "%A" <| wrapper "def x = (2)" // returns null

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