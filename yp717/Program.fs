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
    let inp1 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10)]
    // let inp2 = [TokLit (Int 5)]
    // let inp3 = [TokBuiltInOp ADD]
    // let inp4 = []    
    // // let inp5 = [TokLit (Double 5.5); TokLit (String "hi"); TokLit (Int 10)]    

    // 5 * 10 + 3
    let inp6 = [TokLit (Int 5); TokBuiltInOp MULTIPLY; TokLit (Int 10); TokBuiltInOp ADD; TokLit (Int 3)]

    // // FuncDefExp (funcName, leftTree, rightTree)
    // let f x = y = x + 1 in z = y + 2 in z
    // let 
    let inp7 = [TokSpecOp LET; 
                TokIdentifier (IDString "f");  
                TokSpecOp EQUALS; 
                TokWhitespace (Space);
                TokLit (Int 5); 
                TokWhitespace (Space);
                TokBuiltInOp ADD; 
                TokWhitespace (Space);
                TokLit (Int 1)]

    // // Conditional(condition, leftTree, rightTree)
    // // condition: if 6 < 10
    // // leftTree: 5
    // // rightTree: 10
    let inp8 = [TokSpecOp IF;
                TokSpecOp LRB; 
                TokBuiltInOp LE;
                TokLit (Int 6);
                TokLit (Int 10);
                TokSpecOp RRB;
                TokSpecOp THEN; 
                TokLit (Int 5); 
                TokSpecOp ELSE; 
                TokLit (Int 10)]

    // lambda f -> 3*(f + 10)
    // lambda f -> (*) 3 (f + 10)
    // lambda f -> (((*) 3) (((+) f) 10))
    (* lambda(Identifier "f" 
            FuncApp (
                FuncApp (MULTIPLY, 3), 
                FuncApp (
                    FuncApp (ADD, f), 
                    Const 10
                )
            )
        )
    *)

    // lambda f -> * 3 ((+) f 10)
    // let inp10 = [TokSpecOp LAMBDA;
    //             TokIdentifier (IDString "f");
    //             TokSpecOp ARROWFUNC;
    //             TokLit (Int 3);
    //             TokBuiltInOp MULTIPLY;
    //             TokLit (Int 10);
    //             TokBuiltInOp ADD;
    //             TokIdentifier (IDString "f");]

    (*
        (Conditional
            (FuncApp(FuncApp (BuiltInFunc LE,Const (Int 6)),Const (Int 10)),
            Const (Int 5),
            Const (Int 6)), 
        10)
    *)

    let inp9 = [
                TokSpecOp IF;
                    TokSpecOp LRB;
                        TokLit (Int 6); 
                        TokBuiltInOp LE; 
                        TokLit (Int 10);
                    TokSpecOp RRB;
                TokSpecOp THEN;
                    TokLit (Int 5);
                TokSpecOp ELSE;
                    TokLit (Int 6);
                ]
    
    // this is working now!
    let inp10 = [TokSpecOp LRB;
                TokLit (Int 6);  
                TokBuiltInOp LE;
                TokLit (Int 10);
                TokSpecOp RRB;]

    // this is working now!
    let inp11 = [TokSpecOp LRB;
                TokSpecOp LRB;
                TokSpecOp LRB;
                TokLit (Int 6);  
                TokBuiltInOp ADD;
                TokLit (Int 10);
                TokSpecOp RRB;
                TokSpecOp RRB;
                TokSpecOp RRB;]

    // let m = pMany (pSkipToken (TokSpecOp(IN))) |> ignoreList
    print "\n \n \n"

    pRun pExpr inp7 |> printf "%A"
    // pRun pchainAddSubtract inp6 |> printf "%A"
    // pRun pExpr inp9 |> printf "%A"
    // printf "\n"
    // pRun (pSkipToken (TokSpecOp IN)) [TokBuiltInOp ADD; TokBuiltInOp ADD; TokBuiltInOp ADD] |> printf "%A"

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

