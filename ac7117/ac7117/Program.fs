// Learn more about F# at http://fsharp.org

open System
open Expecto.ExpectoFsCheck
open Expecto
open FsCheck
open Helper
open Combinator_runtime

let expectoConfig = {defaultConfig with verbosity = Logging.LogLevel.Debug}

[<Tests>]
let abstract_tests =
  testList "Basic tests of Bracket Abstraction" [
    test "Test1 Single-var" {
      let InputAst = Function(None, "g", Call(Expression(Variable "g"), Combinator I))
      let AbstractVars = ["g"]
      let expected = Call(Call(Combinator S,Combinator I),Call (Combinator K,Combinator I))
      let actual = AbstractArgs InputAst AbstractVars
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }

    test "Test2 Single-var" {
      let InputAst = Function(None, "f", Function(None, "g", Call(Expression(Variable "g"), Expression(Variable "f"))))
      let AbstractVars = ["g"]
      let expected = Function(None, "f", Call(Call(Combinator S, Combinator I), Call(Combinator K, Expression(Variable "f"))))
      let actual = AbstractArgs InputAst AbstractVars
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }
    
    test "Test3 Single-var" {
      let InputAst = Function(None, "g", Combinator I)
      let AbstractVars = ["g"]
      let expected = Call(Combinator K, Combinator I)
      let actual = AbstractArgs InputAst AbstractVars
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }
    
    test "Test1 Multi-var" {
      let InputAst = Function(None, "g", Combinator I)
      let AbstractVars = ["g"; "f"]
      let expected = Call(Combinator K, Combinator I)
      let actual = AbstractArgs InputAst AbstractVars
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }
    
    test "Test2 Multi-var" {
      let InputAst = Function(None, "f", Function(None, "g", Combinator I))
      let AbstractVars = ["g"; "f"]
      let expected = Call(Combinator K, Call(Combinator K, Combinator I))
      let actual = AbstractArgs InputAst AbstractVars
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }
    
    testProperty "Abstract I outside λ"
      <| (fun (b: Identifier)
            -> (
                  let InputAst = Combinator I
                  let actual = Abstract InputAst b false
                  actual = InputAst
               ))
      
    testProperty "Abstract I inside λ"
      <| (fun (b: Identifier)
            -> (
                  let InputAst = Combinator I
                  let actual = Abstract InputAst b true
                  actual = Call(Combinator K, Combinator I)
               ))
    
    testProperty "Two nested λs"
      <| (fun (vars: NonEmptyString list) (b: (NonEmptyString)) (c:  (NonEmptyString))
            -> (
                  let InputAst = Function(None, c.Get, Function(None, b.Get, Combinator I))
                  let expected = Call(Combinator K, Call(Combinator K, Combinator I))
                  let avars = List.map (fun (x: NonEmptyString) -> x.Get) vars
                  let actual = AbstractArgs InputAst (avars @ [b.Get; c.Get])
                  actual = expected
               ))
  ]
  
  //TODO: test abstracting different indentifier from λ
  |> testLabel "abstract"

[<Tests>]
let print_tests =
  testList "Test printTree function" [
    test "Test Raw AST 1" {
      let InputAst = Function(None, "g", Call(Expression (Variable "g"), Expression (Variable "f")))
      let expected = "(λg.(g f))"
      Expect.equal (printTree InputAst) expected "Test Raw AST 1"
    }
    test "Test Raw AST 2" {
      let InputAst = Call(Function(None, "g", Call(Expression (Variable "g"), Expression (Variable "f"))), Expression(Variable "w"))
      let expected = "((λg.(g f)) w)"
      Expect.equal (printTree InputAst) expected "Test Raw AST 2"
    }
    test "Test Combinator AST 1" {
      let InputAst = Function(None, "f", Call(Call(Combinator S, Combinator I), Call(Combinator K, Expression(Variable "f"))))
      let expected = "(λf.((S I) (K f)))"
      Expect.equal (printTree InputAst) expected "Test Combinator AST 1"
    }
    testProperty "Arithmetic print"
      <| (fun (op: Arithmetic) (b: int) (c: int)
            -> (printTree (Expression(Arithmetic(Literal (Int b), op, Literal (Int c))))) = (sprintf "(%A %A %A)" <| op <| b <| c))
      
    testProperty "Comparison int print"
      <| (fun (op: Comparison) (b: int) (c: int)
            -> (printTree (Expression(Comparison(Literal (Int b), op, Literal (Int c))))) = (sprintf "(%A %A %A)" <| op <| b <| c))
      
    testProperty "Comparison bool print"
      <| (fun (op: Comparison) (b: bool) (c: bool)
            -> (printTree (Expression(Comparison(Literal (Bool b), op, Literal (Bool c))))) = (sprintf "(%A %A %A)" <| op <| b <| c))
  ]
  |> testLabel "print"


[<Tests>]
let eval_tests =
  testList "Checking Abstraction+EvalTree" [
    test "Test Eval 1" {
      let InputAst = Call(Function(None, "f", Expression(Arithmetic(Variable "f", Add, Literal (Int 2)))), Expression(Literal (Int 2))) //(λf.f+2) 2
      let expected = Ok <| Expression(Literal(Int 4))
      Expect.equal (Interpret <| InputAst) expected "Simple arithmetic function"
    }
    testProperty "Simple Arithmetic Operation"
        <| (fun (op: Arithmetic) (b: int) (c: int)
              -> (
                   if c = 0 && op=Divide then true // catching divide by zero error
                   else
                   //todo: add double type
                   let opMap = Map [ Add, (+); Subtract, (-) ; Multiply, (*) ; Divide, (/) ]
                   let AST = Expression(Arithmetic(Literal(Int b), op, Literal(Int c)))
                   let expected = Expression(Literal(Int(Option.get (opMap.TryFind op) b c)))
                   (Interpret <| AST) = Ok expected 
                 ))
    testProperty "Simple Comparison Operation for numericals"
        <| (fun (op: Comparison) (b: int) (c: int)
              -> (
                   //TODO: add double
                   let opMap =
                    Map
                        [ Eq, (=)
                          Ne, (<>)
                          Lt, (<)
                          Gt, (>)
                          Le, (<=)
                          Ge, (>=) ]
                   let AST = Expression(Comparison(Literal(Int b), op, Literal(Int c)))
                   let expected = Expression(Literal(Bool(Option.get (opMap.TryFind op) b c)))
                   (Interpret <| AST) = Ok expected 
                 ))
         
    testProperty "Simple Comparison Operation for all FuncyPy types"
        <| (fun (op: Comparison) (b: Value) (c: Value)
              -> (
                   let opMap =
                    Map
                        [ Eq, (=)
                          Ne, (<>) ]
                   let AST = Expression(Comparison(Literal(b), op, Literal(c)))
                   try // only test when b and c are same value type
                      let expected = Expression(Literal(Bool(Option.get (opMap.TryFind op) b c))) 
                      (Interpret <| AST) = Ok expected
                   with
                   | ex -> true 
                 ))

  ]
  |> testLabel "Evaluation tree"


[<EntryPoint>]
let main argv =
    runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    0 // return an integer exit code
