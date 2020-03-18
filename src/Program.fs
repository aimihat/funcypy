open Expecto
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
            |> Option.map (pRun pExpr)
            |> Option.map Interpret

        match result with
        | Some (Some res) -> printf "%s" <| PrintTree res
        | _ -> printf "Did not find evaluate.\n"
    | _ -> printf "Must enter a .fpy file to execute.\n"

    
//    let tokenTest1 = [TokSpecOp DEF ; TokIdentifier ("f") ; TokIdentifier ("y") ; TokSpecOp EQUALS ; TokWhitespace LineFeed ; TokSpecOp DEF ; TokIdentifier ("x") ; TokSpecOp EQUALS ; TokLit (Int 3) ; TokWhitespace LineFeed ; TokIdentifier ("x") ; TokBuiltInOp (Arithm Multiply) ; TokIdentifier ("y") ; TokWhitespace LineFeed ; TokIdentifier ("f") ; TokLit (Int 2)]
//    printf "%A" <| (pRun pExpr tokenTest1 |> Interpret)

    // function application of lambdas fails (lambda x -> x + 1) 2
//    let tokenTest2 = [TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokLit (Int 2)]
//    printf "%A" <| (pRun pExpr tokenTest2 |> Interpret)

//    let tokenTest3 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//    printf "\n %A" <| (pRun pExpr tokenTest3  |> Interpret)
//    printf "\n \n \n \n"
//    // FuncDefExp('f', Lambda('x', Lambda('y', AST=== 2*x)), Call(Call('f','2'), '3')))
//    let example_5 = """
//    def f x y = 2*x+y
//    f 2 3
//    """
//    let tokenTest4 = [TokSpecOp DEF ; TokIdentifier "f" ; TokIdentifier "x" ; TokIdentifier "y" ; TokSpecOp EQUALS ; TokLit (Int 2) ; TokBuiltInOp (Arithm Multiply) ; TokIdentifier "x" ;  TokBuiltInOp (Arithm Add); TokIdentifier "y" ; TokWhitespace LineFeed ; TokIdentifier "f" ; TokLit (Int 2) ; TokLit (Int 3)]
//    printf "\n %A" <| (pRun pExpr tokenTest4 |> Interpret)

//     let tokenTest5 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest5)

//     let tokenTest6 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest6)

//     let tokenTest7 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest7)

//     let tokenTest8 = [TokSpecOp LRB ; TokSpecOp LAMBDA ; TokIdentifier "x" ; TokSpecOp ARROWFUNC ; TokIdentifier "x" ; TokBuiltInOp (Arithm Add) ; TokLit (Int 1) ; TokSpecOp RRB ; TokLit (Int 2)]
//     printf "%A" <| (pRun pExpr tokenTest8)
    // runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    0