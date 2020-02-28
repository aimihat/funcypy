// Learn more about F# at http://fsharp.org

open System
open Expecto.ExpectoFsCheck
open Expecto
open FsCheck
open Helper
open Combinator_runtime

let expectoConfig = { defaultConfig with verbosity = Logging.LogLevel.Debug }



[<Tests>]
let abstract_tests =
    testList "Basic tests of Bracket Abstraction"
        [ test "Test1 Single-var" {
              let InputAst = Lambda("g", Call(Variable "g", Combinator I))
              let expected = Call(Call(Combinator S, Combinator I), Call(Combinator K, Combinator I))
              let actual = Abstract InputAst
              Expect.equal actual expected (PrintTree actual + " != " + PrintTree expected)
          }

          test "Test3 Single-var" {
              let InputAst = Lambda("g", Combinator I)
              let expected = Call(Combinator K, Combinator I)
              let actual = Abstract InputAst
              Expect.equal actual expected (PrintTree actual + " != " + PrintTree expected)
          }

          test "Test1 Multi-var" {
              let InputAst = Lambda("g", Combinator I)
              let expected = Call(Combinator K, Combinator I)
              let actual = Abstract InputAst
              Expect.equal actual expected (PrintTree actual + " != " + PrintTree expected)
          }

          testProperty "Abstract I outside λ" <| (fun (b: Identifier) ->
          (let InputAst = Combinator I
           let actual = Abstract InputAst
           actual = InputAst))

          testProperty "Abstracted AST contains no λs" <| (fun (A: Ast) ->
          (let actual = Abstract <| InlineDefs A

           let rec containsLambda tree =
               match tree with
               | Lambda(_, _) -> true
               | Call(E1, E2) -> containsLambda E1 || containsLambda E2
               | FuncDefExp(var, body, exp) -> containsLambda body || containsLambda exp
               | E -> false
           containsLambda actual |> not)) ] |> testLabel "abstract"
//
//[<Tests>]
//let print_tests =
//  testList "Test printTree function" [
//    test "Test Raw AST 1" {
//      let InputAst = Function("g", Call(Expression (Variable "g"), Expression (Variable "f")))
//      let expected = "(λg.(g f))"
//      Expect.equal (printTree InputAst) expected "Test Raw AST 1"
//    }
//    test "Test Raw AST 2" {
//      let InputAst = Call(Function("g", Call(Expression (Variable "g"), Expression (Variable "f"))), Expression(Variable "w"))
//      let expected = "((λg.(g f)) w)"
//      Expect.equal (printTree InputAst) expected "Test Raw AST 2"
//    }
//    test "Test Combinator AST 1" {
//      let InputAst = Function("f", Call(Call(Combinator S, Combinator I), Call(Combinator K, Expression(Variable "f"))))
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
    testList "End-to-end tests (abstraction+eval)"
        [ testProperty "Arith int: b op c" <| (fun (op: ArithmeticType) (b: int) (c: int) ->
          (if c = 0 && op = Divide then
              true // catching divide by zero error
           else
               //todo: add double type
               let opMap =
                   Map
                       [ Add, (+)
                         Subtract, (-)
                         Multiply, (*)
                         Divide, (/) ]

               let AST =
                   Call(Call(BuiltInFunc(Arithmetic op), Literal(Int b)), Literal(Int c))
               let expected = Literal(Int(Option.get (opMap.TryFind op) b c))
               (Interpret <| AST) = expected))

          testProperty "Arith double: b op c" <| (fun (op: ArithmeticType) (b: NormalFloat) (c: NormalFloat) ->
          (if c.Get = 0.0 && op = Divide then
              true // catching divide by zero error
           else
               //todo: add double type
               let opMap =
                   Map
                       [ Add, (+)
                         Subtract, (-)
                         Multiply, (*)
                         Divide, (/) ]

               let AST =
                   Call(Call(BuiltInFunc(Arithmetic op), Literal(Double b.Get)), Literal(Double c.Get))
               let expected = Literal(Double(Option.get (opMap.TryFind op) b.Get c.Get))
               (Interpret <| AST) = expected))

          testProperty "Arith mixed num: b op c" <| (fun (op: ArithmeticType) (b: NormalFloat) (c: int) ->
          (if c = 0 && op = Divide then
              true // catching divide by zero error
           else
               let opMap =
                   Map
                       [ Add, (+)
                         Subtract, (-)
                         Multiply, (*)
                         Divide, (/) ]

               let AST =
                   Call(Call(BuiltInFunc(Arithmetic op), Literal(Double b.Get)), Literal(Int c))
               let expected = Literal(Double(Option.get (opMap.TryFind op) b.Get (float c)))
               (Interpret <| AST) = expected))

          testProperty "Arith: (λf.f op b) c" <| (fun (op: ArithmeticType) (b: int) (c: int) ->
          (if b = 0 && op = Divide then
              true // catching divide by zero error
           else
               //todo: add double type
               let opMap =
                   Map
                       [ Add, (+)
                         Subtract, (-)
                         Multiply, (*)
                         Divide, (/) ]

               let InputAst =
                   Call
                       (Lambda("f", Call(Call(BuiltInFunc(Arithmetic op), Variable "f"), Literal(Int b))),
                        Literal(Int c)) //(λf.f+2) 2
               let expected = Literal(Int(Option.get (opMap.TryFind op) c b))
               (Interpret <| InputAst) = expected))

          testProperty "a: bool, b: bool -> (λf.(λg.f comp g) a) b" <| (fun (op_select: bool) (a: bool) (b: bool) ->
          (let op, operator =
              if op_select then Eq, (=)
              else Ne, (<>)

           let AST =
               Call
                   (Lambda
                       ("f",
                        Call
                            (Lambda("g", Call(Call(BuiltInFunc(Comparison op), Variable("f")), Variable("g"))),
                             Literal(Bool a))), Literal(Bool b))
           let expected = Literal(Bool(operator a b))
           (Interpret <| AST) = expected))

          testProperty "Simple Comparison Operation for numericals" <| (fun (op: ComparisonType) (b: int) (c: int) ->
          (let opMap =
              Map
                  [ Eq, (=)
                    Ne, (<>)
                    Lt, (<)
                    Gt, (>)
                    Le, (<=)
                    Ge, (>=) ]

           let AST =
               Call(Call(BuiltInFunc(Comparison op), Literal(Int b)), Literal(Int c))
           let expected = Literal(Bool(Option.get (opMap.TryFind op) b c))
           (Interpret <| AST) = expected))

          testProperty "x = a, x + b" <| (fun (a: int) (b: int) ->
          (let AST =
              FuncDefExp("x", Literal(Int a), Call(Call(BuiltInFunc(Arithmetic Add), Variable "x"), Literal(Int b)))
           (Interpret <| AST) = Literal(Int(a + b))))

          testProperty "(λx.y=x, x+y) a" <| (fun (a: int) ->
          (let AST =
              Call
                  (Lambda
                      ("x",
                       FuncDefExp
                           ("y", Variable "x", Call(Call(BuiltInFunc(Arithmetic Add), Variable "x"), Variable "y"))),
                   Literal(Int a))
           (Interpret <| AST) = Literal(Int(2 * a))))

          testProperty "(λx.(if x then a else b)) c:bool" <| (fun (a: CombinatorType) (b: CombinatorType) (c: bool) ->
          (let AST =
              Call
                  (Lambda
                      ("x", Call(Call(Call(BuiltInFunc IfThenElse, Variable "x"), Combinator a), Combinator b)),
                   Literal(Bool(c)))
           (Interpret <| AST) =
               if c then Combinator a
               else Combinator b))

          testProperty "(λx.(if x then a else b)) c:int" <| (fun (a: CombinatorType) (b: CombinatorType) (c: int) ->
          (let AST =
              Call
                  (Lambda
                      ("x", Call(Call(Call(BuiltInFunc IfThenElse, Variable "x"), Combinator a), Combinator b)),
                   Literal(Int(c)))
           (Interpret <| AST) =
               if c <> 0 then Combinator a
               else Combinator b))

          testProperty "(λx.(if x then a else b)) c:double" <| (fun (a: CombinatorType) (b: CombinatorType) (c: NormalFloat) ->
          (let AST =
              Call
                  (Lambda
                      ("x", Call(Call(Call(BuiltInFunc IfThenElse, Variable "x"), Combinator a), Combinator b)),
                   Literal(Double(c.Get)))
           (Interpret <| AST) =
               if c.Get <> 0.0 then Combinator a
               else Combinator b))
          testProperty "(str1+str2) c:double" <| (fun (str1: string) (str2: string) ->
          (let AST = Call(Call(BuiltInFunc(Arithmetic Add), Literal(String str1)), Literal(String str2))
           (Interpret <| AST) = Literal(String(str1 + str2))))
          testProperty "implode [a, b, c]" <| (fun (a: string) (b: string) (c: string) ->
          (let AST =
              Call
                  (BuiltInFunc(ListF ImplodeString),
                   Pair(Literal(String a), Pair(Literal(String b), Pair(Literal(String c), Null))))
           (Interpret <| AST) = Literal(String(a + b + c))))
          
          test "head [1+1,2] = 2" {
              let AST = Call(BuiltInFunc (ListF Head), Pair(Call(Call(BuiltInFunc (Arithmetic Add), Literal (Int 4)), Literal (Int 2)), Literal (Int 2)))
              let actual = (Interpret <| AST)
              Expect.equal actual (Literal(Int 6)) (PrintTree actual + " != 6")
          }
          
          test "P (1+1) 3 != [2,3]" {
              let AST = Call(Call(BuiltInFunc (ListF P), Call(Call(BuiltInFunc (Arithmetic Add), Literal(Int 1)), Literal(Int 1))), Literal(Int 3))
              let actual = (Interpret <| AST)
              Expect.equal actual ( Pair (Literal (Int 2),Pair (Literal (Int 3),Null))) (PrintTree actual + " != [2,3]")
          }
          
          testProperty "head ([ast,ast,...])" <| (fun (l: Value list) ->
          (let list = l @ [ Int 1 ] |> List.filter (function | Double x -> not (Double.IsNaN x) | _ -> true) |> List.map (fun i -> Literal i)
           let AST = Call(BuiltInFunc(ListF Head), ListFromPairs list)
           (Interpret <| AST) = list.[0]))
          
          test "tail ([1,2,3])" {
              let AST =
                  Call(BuiltInFunc(ListF Tail), Pair(Literal(Int 1), Pair(Literal(Int 2), Pair(Literal(Int 2), Null))))
              let actual = (Interpret <| AST)
              Expect.equal actual (Pair(Literal(Int 2), Pair(Literal(Int 2), Null))) (PrintTree actual + " != [2,3]")
          }
          
          test "islist [1,2]" {
              let AST = Call(BuiltInFunc (ListF IsList), Pair(Literal (Int 1), Literal (Int 2)))
              let actual = (Interpret <| AST)
              Expect.equal actual (Literal(Bool true)) (PrintTree actual + " != true")
          }
          
          testProperty "islist val = false" <| (fun (s: Value) ->
          (let AST = Call(BuiltInFunc (ListF IsList), Literal s)
           (Interpret <| AST) = Literal(Bool false)))


//          test "5!" {
//              let fact =
//                  FuncDefExp
//                      ("fact'",
//                       Call(Combinator Y, Function("fact", Function
//                           ("n",
//                            Call
//                                (Call
//                                    (Call(BuiltInFunc IfThenElse, Variable "n"),
//                                     Call
//                                         (Call(BuiltInFunc(Arithmetic Multiply), Variable "n"),
//                                          Call
//                                              (Variable "fact",
//                                               Call(Call(BuiltInFunc(Arithmetic Subtract), Variable "n"), Literal(Int 1))))), Literal(Int 1))))),
//                           Call(Variable "fact'", Literal(Int 5)))
//              // Convert AST
//              let actual = (Interpret <| fact)
//              Expect.equal actual (Literal(Int 120)) (PrintTree actual + " != 120")
//          }

          testProperty "isempty ([ast,ast,...])" <| (fun (l: Value list) ->
          (let list = l @ [ Int 1 ] |> List.map (fun i -> Literal i)
           let AST = Call(BuiltInFunc(ListF IsEmpty), ListFromPairs list)
           (Interpret <| AST) = Literal(Bool(list.IsEmpty))))

          testProperty "implode (explode str) = str" <| (fun (s: NonEmptyString) ->
          (let AST =
              Call(BuiltInFunc(ListF ImplodeString), Call(BuiltInFunc(ListF ExplodeString), Literal(String s.Get)))
           (Interpret <| AST) = Literal(String s.Get))) ] |> testLabel "Evaluation tree"


[<EntryPoint>]
let main argv =
//    printf "%A" (Interpret <| fact)
    runTestsInAssemblyWithCLIArgs [] [||] |> ignore
    0 // return an integer exit code
