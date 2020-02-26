open System
// open Tokeniser
open Common
open Parser 

// mock parser example from Don Syme
let src2 = "2+5"
let print x = printfn "%A" x

[<EntryPoint>]
let main(argv) =
    // // builtinPlus a b -> FuncApp(FuncApp(builtinPlus, a), b)
    // let inp1 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10)]
    // let inp2 = [TokLit (Int 5)]
    // let inp3 = [TokBuiltInOp ADD]
    // let inp4 = []    
    // // let inp5 = [TokLit (Double 5.5); TokLit (String "hi"); TokLit (Int 10)]    

    // let inp6 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10); TokBuiltInOp MULTIPLY; TokLit (Int 3)]

    // // FuncDefExp (funcName, leftTree, rightTree)
    // let f x = y = x + 1 in z = y + 2 in z
    // let 
    let inp7 = [TokSpecOp LET; 
                TokIdentifier (IDString "f"); 
                TokSpecOp EQUALS; 
                TokLit (Int 5); 
                TokBuiltInOp ADD; 
                TokLit (Int 1)]

    // // Conditional(condition, leftTree, rightTree)
    // // condition: if 6 < 10
    // // leftTree: 5
    // // rightTree: 10
    // let inp8 = [TokSpecOp IF; 
    //             TokLit (Int 6); 
    //             TokBuiltInOp LE; 
    //             TokLit (Int 10); 
    //             TokSpecOp THEN; 
    //             TokLit (Int 5); 
    //             TokSpecOp ELSE; 
    //             TokLit (Int 10)]

    // let inp9 = [TokSpecOp IF; 
    //             TokLit (Int 6); 
    //             TokBuiltInOp LE; 
    //             TokLit (Int 10); 
    //             TokSpecOp THEN; 
    //             TokLit (Int 5)]
    
    // pRun pExpr inp7 |> printf "%A"
    // printf "\n"
    pRun (pSkipToken (TokSpecOp IN)) [TokBuiltInOp ADD; TokBuiltInOp ADD; TokBuiltInOp ADD] |> printf "%A"

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

