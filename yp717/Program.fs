open System
// open Tokeniser
open Common
open Parser 

// mock parser example from Don Syme
let src2 = "2+5"
let print x = printfn "%A" x

[<EntryPoint>]
let main(argv) =
    // builtinPlus a b -> FuncApp(FuncApp(builtinPlus, a), b)
    let inp1 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10)]
    let inp2 = [TokLit (Int 5)]
    let inp3 = [TokBuiltInOp ADD]
    let inp4 = []    
    // let inp5 = [TokLit (Double 5.5); TokLit (String "hi"); TokLit (Int 10)]    

    let inp6 = [TokLit (Int 5); TokBuiltInOp ADD; TokLit (Int 10); TokBuiltInOp MULTIPLY; TokLit (Int 3)]

    // FuncDefExp (funcName, leftTree, rightTree)
    let inp7 = [TokSpecOp FUN; 
                TokIdentifier (IDString "f"); 
                TokSpecOp EQUALS; 
                TokLit (Int 5); 
                TokBuiltInOp ADD; 
                TokLit (Int 1)]

    // Conditional(condition, leftTree, rightTree)
    // condition: if 6 < 10
    // leftTree: 5
    // rightTree: 10
    let inp8 = [TokSpecOp IF; 
                TokLit (Int 6); 
                TokBuiltInOp LE; 
                TokLit (Int 10); 
                TokSpecOp THEN; 
                TokLit (Int 5); 
                TokSpecOp ELSE; 
                TokLit (Int 10)]

    let inp9 = [TokSpecOp IF; 
                TokLit (Int 6); 
                TokBuiltInOp LE; 
                TokLit (Int 10); 
                TokSpecOp THEN; 
                TokLit (Int 5)]
    
    let p = 
        parser {
            let! first = pToken
            let! second = pToken
            let! third = pToken
            return first, second, third
        }

    // pRun pLiteral inp5 |> printfn "%A"
    // pRun (pMany pLiteral) inp5 |> printfn "%A"
    // pRun (pMany pAST) inp1 |> printfn "%A"
    // pRun (pMany pAST) inp6 |> printfn "%A"

    // Conditional(condition<Ast>, ifTrue<Ast>, ifFalse<Ast>)
    // pRun pAST inp8 |> printf "%A"

    // let getResult tuple =
    //     match tuple with
    //     | Some (condition:list<Ast>, ifTrue:list<Ast>, ifFalse:list<Ast>, integer:int32) ->
    //         (Conditional(condition.Head, ifTrue.Head, ifFalse.Head), integer)
    // have to break down each of condition ifTrue and ifFalse

    pRun pIf inp9 |> printf "%A"

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

