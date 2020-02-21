// Learn more about F# at http://fsharp.org

open System
open FsCheck
open Expecto

open Helper
open Combinator_runtime

let expectoConfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}

let InputAst = Lambda("f", Lambda("g", Combinator I))
let AbstractVars = [Variable "g"; Variable "f"]
let expected = FuncApp(Combinator K, FuncApp(Combinator K, Combinator I))
sprintf "%A" (AbstractArgs InputAst AbstractVars)

[<Tests>]
let tests =
  testList "Basic tests of Bracket Abstraction" [
    test "Test1 Single-var" {
      let InputAst = Lambda("g", FuncApp(Variable "g", Combinator I))
      let AbstractVars = [Variable "g"]
      let expected = FuncApp(FuncApp (Combinator S,Combinator I),FuncApp (Combinator K,Combinator I))
      Expect.equal expected (AbstractArgs InputAst AbstractVars) ""
    }

    test "Test2 Single-var" {
      let InputAst = Lambda("f", Lambda("g", FuncApp(Variable "g", Variable "f")))
      let AbstractVars = [Variable "g"]
      let expected = Lambda("f", FuncApp(FuncApp(Combinator S, Combinator I), FuncApp(Combinator K, Variable "f")))
      Expect.equal expected (AbstractArgs InputAst AbstractVars) ""
    }
    
    test "Test3 Multi-var" {
      let InputAst = Lambda("f", Lambda("g", Combinator I))
      let AbstractVars = [Variable "g"; Variable "f"]
      let expected = FuncApp(Combinator K, FuncApp(Combinator K, Combinator I))
      Expect.equal expected (AbstractArgs InputAst AbstractVars) ""
    }
  ]
  |> testLabel "samples"


[<EntryPoint>]
let main argv =
    runTestsInAssembly expectoConfig [||] |> ignore
    let Ast = Lambda("f", Lambda("g", FuncApp(Variable "g", Variable "f")))
    sprintf "Combinator expression:\n %A" <| Abstract (Abstract Ast (Variable "g")) (Variable "f")
    let Ast2 = Lambda("f", FuncApp(Combinator K, Variable "f"))
    sprintf "Combinator expression:\n %A" <| Abstract Ast2 (Variable "f")
    sprintf "Combinator expression:\n %A" <| Abstract (Combinator I) (Variable "f")
    0 // return an integer exit code
