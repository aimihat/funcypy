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
      let expected = Call(Call(Combinator S,Combinator I),Call (Combinator K,Combinator I))
      let actual = Abstract InputAst
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }

    test "Test3 Single-var" {
      let InputAst = Function(None, "g", Combinator I)
      let expected = Call(Combinator K, Combinator I)
      let actual = Abstract InputAst
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }
    
    test "Test1 Multi-var" {
      let InputAst = Function(None, "g", Combinator I)
      let expected = Call(Combinator K, Combinator I)
      let actual = Abstract InputAst
      Expect.equal actual expected (printTree actual + " != " + printTree expected)
    }
    
    testProperty "Abstract I outside λ"
      <| (fun (b: Identifier)
            -> (
                  let InputAst = Combinator I
                  let actual = Abstract InputAst
                  actual = InputAst
               ))
 
  ]
  
  //TODO: test abstracting different indentifier from λ
  |> testLabel "abstract"
//
//[<Tests>]
//let print_tests =
//  testList "Test printTree function" [
//    test "Test Raw AST 1" {
//      let InputAst = Function(None, "g", Call(Expression (Variable "g"), Expression (Variable "f")))
//      let expected = "(λg.(g f))"
//      Expect.equal (printTree InputAst) expected "Test Raw AST 1"
//    }
//    test "Test Raw AST 2" {
//      let InputAst = Call(Function(None, "g", Call(Expression (Variable "g"), Expression (Variable "f"))), Expression(Variable "w"))
//      let expected = "((λg.(g f)) w)"
//      Expect.equal (printTree InputAst) expected "Test Raw AST 2"
//    }
//    test "Test Combinator AST 1" {
//      let InputAst = Function(None, "f", Call(Call(Combinator S, Combinator I), Call(Combinator K, Expression(Variable "f"))))
//      let expected = "(λf.((S I) (K f)))"
//      Expect.equal (printTree InputAst) expected "Test Combinator AST 1"
//    }
//    testProperty "Arithmetic print"
//      <| (fun (op: Arithmetic) (b: int) (c: int)
//            -> (printTree (Expression(Arithmetic(Literal (Int b), op, Literal (Int c))))) = (sprintf "(%A %A %A)" <| op <| b <| c))
//      
//    testProperty "Comparison int print"
//      <| (fun (op: Comparison) (b: int) (c: int)
//            -> (printTree (Expression(Comparison(Literal (Int b), op, Literal (Int c))))) = (sprintf "(%A %A %A)" <| op <| b <| c))
//      
//    testProperty "Comparison bool print"
//      <| (fun (op: Comparison) (b: bool) (c: bool)
//            -> (printTree (Expression(Comparison(Literal (Bool b), op, Literal (Bool c))))) = (sprintf "(%A %A %A)" <| op <| b <| c))
//  ]
//  |> testLabel "print"


[<Tests>]
let eval_tests =
  testList "End-to-end tests (abstraction+eval)" [
    testProperty "Arith: b op c"
        <| (fun (op: ArithmeticType) (b: int) (c: int)
              -> (
                   if c = 0 && op=Divide then true // catching divide by zero error
                   else
                   //todo: add double type
                   let opMap = Map [ Add, (+); Subtract, (-) ; Multiply, (*) ; Divide, (/) ]
                   let AST = Call(Call(Expression (BuiltInFunc (Arithmetic op)), Expression(Literal(Int b))), Expression(Literal(Int c)))
                   let expected = Expression(Literal(Int(Option.get (opMap.TryFind op) b c)))
                   (Interpret <| AST) = expected 
                 ))
    testProperty "Arith: (λf.f op b) c"
        <| (fun (op: ArithmeticType) (b: int) (c: int)
              -> (
                   if b = 0 && op=Divide then true // catching divide by zero error
                   else
                   //todo: add double type
                   let opMap = Map [ Add, (+); Subtract, (-) ; Multiply, (*) ; Divide, (/) ]
                   let InputAst = Call(Function(None, "f", Call(Call(Expression(BuiltInFunc(Arithmetic op)), Expression(Variable "f")), Expression(Literal (Int b)))), Expression(Literal (Int c))) //(λf.f+2) 2
                   let expected = Expression(Literal(Int(Option.get (opMap.TryFind op) c b  )))
                   (Interpret <| InputAst) = expected 
                 ))
        
    testProperty "a: bool, b: bool -> (λf.(λg.f comp g) a) b"
    <| (fun (op_select: bool) (a: bool) (b: bool)
          -> (
               let op, operator =
                 if op_select then
                   Eq, (=) else Ne, (<>)
               let AST = Call(Function(None, "f", Call(Function(None, "g", Call(Call(Expression(BuiltInFunc (Comparison op)),Expression(Variable("f"))), Expression(Variable("g")))),Expression(Literal(Bool a)))), Expression(Literal(Bool b)))
               let expected = Expression(Literal(Bool(operator a b)))
               (Interpret <| AST) = expected 
             ))

    testProperty "Simple Comparison Operation for numericals"
        <| (fun (op: ComparisonType) (b: int) (c: int)
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
                   let AST = Call(Call(Expression (BuiltInFunc (Comparison op)), Expression(Literal(Int b))), Expression(Literal(Int c)))
                   let expected = Expression(Literal(Bool(Option.get (opMap.TryFind op) b c)))
                   (Interpret <| AST) = expected 
                 )) ]
  |> testLabel "Evaluation tree"


[<EntryPoint>]
let main argv =
    runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    0 // return an integer exit code
