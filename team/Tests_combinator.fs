module Test_runtime
//
//open System
//open Expecto.ExpectoFsCheck
//open Expecto
//open FsCheck
//open Helper
//open Combinator_runtime
//
//let EqualIgnoreID actual expected out =
//    // Checks that 2 AST structures are identical
//    // Ignoring any ID
//    let rec IdenticalAst actual expected =
//        match (actual, expected) with
//        | Call(e1, e2, _), Call(e3, e4, _) ->
//            (IdenticalAst e1 e3) && (IdenticalAst e2 e4)
//        | Pair(e1, e2, _), Pair(e3, e4, _) ->
//            (IdenticalAst e1 e3) && (IdenticalAst e2 e4)
//        | other1, other2 ->
//            other1 = other2
//    Expect.isTrue (IdenticalAst actual expected) out
//
//[<Tests>]
//let abstract_tests =
//    testList "Basic tests for Bracket Abstraction"
//        [ test "Test1 Single-var" {
//              let InputAst = Lambda("g", DCall(Variable "g", Combinator I))
//              let expected = DCall(DCall(Combinator S, Combinator I), DCall(Combinator K, Combinator I))
//              let actual = Abstract InputAst
//              EqualIgnoreID actual expected (PrintTree actual + " != " + PrintTree expected)
//          }
//
//          test "Test2 Single-var" {
//              let InputAst = Lambda("g", Combinator I)
//              let expected = DCall(Combinator K, Combinator I)
//              let actual = Abstract InputAst
//              EqualIgnoreID actual expected (PrintTree actual + " != " + PrintTree expected)
//          }
//
//          testProperty "Abstract I outside λ" <| (fun (b: Identifier) ->
//          (let InputAst = Combinator I
//           let actual = Abstract InputAst
//           actual = InputAst))
//
//          testProperty "Abstracted AST contains no λs" <| (fun (A: Ast) ->
//          (let actual = Abstract <| InlineDefs A
//
//           let rec containsLambda tree =
//               match tree with
//               | Lambda(_, _) -> true
//               | Call(E1, E2, _) -> containsLambda E1 || containsLambda E2
//               | FuncDefExp(_, body, exp) -> containsLambda body || containsLambda exp
//               | _ -> false
//           containsLambda actual |> not)) ] |> testLabel "abstract"
//
//[<Tests>]
//let print_tests = // Low-priority test
//    testList "Test printTree function"
//        [ test "Test Raw AST 1" {
//              let InputAst = Lambda("g", DCall(Variable "g", Variable "f"))
//              let expected = "(λg.(g f))"
//              Expect.equal (PrintTree InputAst) expected "Test Raw AST 1"
//          }
//          test "Test Raw AST 2" {
//              let InputAst = DCall(Lambda("g", DCall(Variable "g", Variable "f")), Variable "w")
//              let expected = "((λg.(g f)) w)"
//              Expect.equal (PrintTree InputAst) expected "Test Raw AST 2"
//          }
//          test "Test Combinator AST 1" {
//              let InputAst = Lambda("f", DCall(DCall(Combinator S, Combinator I), DCall(Combinator K, Variable "f")))
//              let expected = "(λf.((S I) (K f)))"
//              Expect.equal (PrintTree InputAst) expected "Test Combinator AST 1"
//          }
//          test "Test List" {
//              let InputAst = DPair(Literal(Int 1), DPair(Literal(Int 2), DPair(Literal(Int 3), Null)))
//              let expected = "[1, 2, 3]"
//              Expect.equal (PrintTree InputAst) expected "Test List"
//          }
//          //TODO: add more print tests
//         ] |> testLabel "print"
//
//
//[<Tests>]
//let eval_tests =
//    testList "End-to-end tests (abstraction+eval)"
//        [ testProperty "Arith int: b op c" <| (fun (op: ArithmeticType) (b: int) (c: int) ->
//          (if c = 0 && op = Divide then
//              true // catching divide by zero error
//           else
//               let opMap = Map [ Add, (+)
//                                 Subtract, (-)
//                                 Multiply, (*)
//                                 Divide, (/) ]
//
//               let AST = DCall(DCall(BuiltInFunc(Arithm op), Literal(Int b)), Literal(Int c))
//               let expected = Literal(Int(Option.get (opMap.TryFind op) b c))
//               (Interpret <| AST) = expected))
//
//          testProperty "Arith double: b op c" <| (fun (op: ArithmeticType) (b: NormalFloat) (c: NormalFloat) ->
//          (if c.Get = 0.0 && op = Divide then
//              true // catching divide by zero error
//           else
//               let opMap = Map [ Add, (+)
//                                 Subtract, (-)
//                                 Multiply, (*)
//                                 Divide, (/) ]
//
//               let AST = DCall(DCall(BuiltInFunc(Arithm op), Literal(Double b.Get)), Literal(Double c.Get))
//               let expected = Literal(Double(Option.get (opMap.TryFind op) b.Get c.Get))
//               (Interpret <| AST) = expected))
//
//          testProperty "Arith mixed types: b op c" <| (fun (op: ArithmeticType) (b: NormalFloat) (c: int) ->
//          (if c = 0 && op = Divide then
//              true // catching divide by zero error
//           else
//               let opMap =
//                   Map
//                       [ Add, (+)
//                         Subtract, (-)
//                         Multiply, (*)
//                         Divide, (/) ]
//
//               let AST =
//                   DCall(DCall(BuiltInFunc(Arithm op), Literal(Double b.Get)), Literal(Int c))
//               let expected = Literal(Double(Option.get (opMap.TryFind op) b.Get (float c)))
//               (Interpret <| AST) = expected))
//       
//          testProperty "(str1+str2) c:double" <| (fun (str1: string) (str2: string) ->
//          (let AST = DCall(DCall(BuiltInFunc(Arithm Add), Literal(String str1)), Literal(String str2))
//           (Interpret <| AST) = Literal(String(str1 + str2))))
//        
//          testProperty "Arith: (λf.f op b) c" <| (fun (op: ArithmeticType) (b: int) (c: int) ->
//          (if b = 0 && op = Divide then
//              true // catching divide by zero error
//           else
//               let opMap = Map [ Add, (+)
//                                 Subtract, (-)
//                                 Multiply, (*)
//                                 Divide, (/) ]
//
//               let InputAst = DCall(Lambda("f", DCall(DCall(BuiltInFunc(Arithm op), Variable "f"), Literal(Int b))), Literal(Int c)) //(λf.f+2) 2
//               let expected = Literal(Int(Option.get (opMap.TryFind op) c b))
//               (Interpret <| InputAst) = expected))
//
//          testProperty "a: bool, b: bool -> (λf.(λg.f comp g) a) b" <| (fun (op_select: bool) (a: bool) (b: bool) ->
//          (let op, operator =
//              if op_select then Eq, (=)
//              else Ne, (<>)
//
//           let AST =
//               DCall
//                   (Lambda
//                       ("f",
//                        DCall
//                            (Lambda("g", DCall(DCall(BuiltInFunc(Comp op), Variable("f")), Variable("g"))),
//                             Literal(Bool a))), Literal(Bool b))
//           let expected = Literal(Bool(operator a b))
//           (Interpret <| AST) = expected))
//
//          testProperty "Simple Comparison Operation for numericals" <| (fun (op: ComparisonType) (b: int) (c: int) ->
//          (let opMap =
//              Map
//                  [ Eq, (=)
//                    Ne, (<>)
//                    Lt, (<)
//                    Gt, (>)
//                    Le, (<=)
//                    Ge, (>=) ]
//
//           let AST = DCall(DCall(BuiltInFunc(Comp op), Literal(Int b)), Literal(Int c))
//           let expected = Literal(Bool(Option.get (opMap.TryFind op) b c))
//           (Interpret <| AST) = expected))
//
//          // Testing FuncDefExp below
//          testProperty "x = a, x + b" <| (fun (a: int) (b: int) ->
//          (let AST = FuncDefExp("x", Literal(Int a), DCall(DCall(BuiltInFunc(Arithm Add), Variable "x"), Literal(Int b)))
//           (Interpret <| AST) = Literal(Int(a + b))))
//
//          testProperty "(λx.y=x, x+y) a" <| (fun (a: int) ->
//          (let AST =
//              DCall
//                  (Lambda
//                      ("x",
//                       FuncDefExp("y", Variable "x", DCall(DCall(BuiltInFunc(Arithm Add), Variable "x"), Variable "y"))),
//                   Literal(Int a))
//           (Interpret <| AST) = Literal(Int(2 * a))))
//
//          // Testing IfThenElse
//          testProperty "(λx.(if x then a else b)) c:bool" <| (fun (a: CombinatorType) (b: CombinatorType) (c: bool) ->
//          (let AST =
//              DCall
//                  (Lambda("x", DCall(DCall(DCall(BuiltInFunc IfThenElse, Variable "x"), Combinator a), Combinator b)),
//                   Literal(Bool(c)))
//           (Interpret <| AST) =
//               if c then Combinator a
//               else Combinator b))
//
//          testProperty "(λx.(if x then a else b)) c:int" <| (fun (a: CombinatorType) (b: CombinatorType) (c: int) ->
//          (let AST =
//              DCall
//                  (Lambda("x", DCall(DCall(DCall(BuiltInFunc IfThenElse, Variable "x"), Combinator a), Combinator b)),
//                   Literal(Int(c)))
//           (Interpret <| AST) =
//               if c <> 0 then Combinator a
//               else Combinator b))
//
//          testProperty "(λx.(if x then a else b)) c:double" <| (fun (a: CombinatorType) (b: CombinatorType) (c: NormalFloat) ->
//          (let AST =
//              DCall
//                  (Lambda("x", DCall(DCall(DCall(BuiltInFunc IfThenElse, Variable "x"), Combinator a), Combinator b)),
//                   Literal(Double(c.Get)))
//           (Interpret <| AST) =
//               if c.Get <> 0.0 then Combinator a
//               else Combinator b))
//
//          // Testing List Built-ins
//          testProperty "implode (explode str) = str" <| (fun (s: NonEmptyString) ->
//          (let AST = DCall(BuiltInFunc(ListF ImplodeStr), DCall(BuiltInFunc(ListF ExplodeStr), Literal(String s.Get)))
//           (Interpret <| AST) = Literal(String s.Get)))
//          
//          testProperty "implode [a, b, c]" <| (fun (a: string) (b: string) (c: string) ->
//          (let AST =
//              DCall
//                  (BuiltInFunc(ListF ImplodeStr),
//                   DPair(Literal(String a), DPair(Literal(String b), DPair(Literal(String c), Null))))
//           (Interpret <| AST) = Literal(String(a + b + c))))
//
//          test "head [1+1,2] = 2" {
//              let AST =
//                  DCall
//                      (BuiltInFunc(ListF Head),
//                       DPair(DCall(DCall(BuiltInFunc(Arithm Add), Literal(Int 4)), Literal(Int 2)), Literal(Int 2)))
//              let actual = (Interpret <| AST)
//              EqualIgnoreID actual (Literal(Int 6)) (PrintTree actual + " != 6")
//          }
//
//          test "P (1+1) 3 != [2,3]" {
//              let AST =
//                  DCall
//                      (DCall
//                          (BuiltInFunc(ListF P), DCall(DCall(BuiltInFunc(Arithm Add), Literal(Int 1)), Literal(Int 1))),
//                       Literal(Int 3))
//              let actual = (Interpret <| AST)
//              EqualIgnoreID actual (DPair(Literal(Int 2), DPair(Literal(Int 3), Null)))
//                  (PrintTree actual + " != [2,3]")
//          }
//
//          testProperty "head ([ast,ast,...])" <| (fun (l: Value list) ->
//          (let list =
//              l @ [ Int 1 ]
//              |> List.filter (function
//                  | Double x -> not (Double.IsNaN x)
//                  | _ -> true)
//              |> List.map (fun i -> Literal i)
//
//           let AST = DCall(BuiltInFunc(ListF Head), ListFromPairs list)
//           (Interpret <| AST) = list.[0]))
//
//          test "tail ([1,2,3])" {
//              let AST =
//                  DCall
//                      (BuiltInFunc(ListF Tail),
//                       DPair(Literal(Int 1), DPair(Literal(Int 2), DPair(Literal(Int 2), Null))))
//              let actual = (Interpret <| AST)
//              EqualIgnoreID actual (DPair(Literal(Int 2), DPair(Literal(Int 2), Null)))
//                  (PrintTree actual + " != [2,3]")
//          }
//
//          test "islist [1,2]" {
//              let AST = DCall(BuiltInFunc(ListF IsList), DPair(Literal(Int 1), Literal(Int 2)))
//              let actual = (Interpret <| AST)
//              EqualIgnoreID actual (Literal(Bool true)) (PrintTree actual + " != true")
//          }
//
//          testProperty "islist val = false" <| (fun (s: Value) ->
//          (let AST = DCall(BuiltInFunc(ListF IsList), Literal s)
//           (Interpret <| AST) = Literal(Bool false)))
//
//          // Testing recursion
//          //          test "5!" {
//          //              let fact =
//          //                  FuncDefExp
//          //                      ("fact'",
//          //                       Call(Combinator Y, Function("fact", Function
//          //                           ("n",
//          //                            Call
//          //                                (Call
//          //                                    (Call(BuiltInFunc IfThenElse, Variable "n"),
//          //                                     Call
//          //                                         (Call(BuiltInFunc(Arithmetic Multiply), Variable "n"),
//          //                                          Call
//          //                                              (Variable "fact",
//          //                                               Call(Call(BuiltInFunc(Arithmetic Subtract), Variable "n"), Literal(Int 1))))), Literal(Int 1))))),
//          //                           Call(Variable "fact'", Literal(Int 5)))
//          //              // Convert AST
//          //              let actual = (Interpret <| fact)
//          //              Expect.equal actual (Literal(Int 120)) (PrintTree actual + " != 120")
//          //          }
//
//          testProperty "isempty ([ast,ast,...])" <| (fun (l: Value list) ->
//          (let list =
//              l @ [ Int 1 ]
//              |> List.filter (function
//                  | Double i -> (System.Double.IsNaN >> not) i // NaN value is invalid
//                  | _ -> true)
//              |> List.map (fun i -> Literal i)
//
//           let AST = DCall(BuiltInFunc(ListF IsEmpty), ListFromPairs list)
//           (Interpret <| AST) = Literal(Bool(list.IsEmpty)))) ] |> testLabel "Evaluation tree"
