// module Parser

// open Common
// // open Tokeniser

// // Given Examples
// // builtinPlus a b -> FuncApp(FuncApp(builtinPlus, a), b)
// // a + b -> FuncApp(FuncApp(builtinPlus, a), b)
// // a * b -> FuncApp(FuncApp(builtinTimes, a), b)
// // (a+b)*c) -> FuncApp(FuncApp(builtinTimes, FuncApp(FuncApp(builtinPlus,a),b)),c)

// let print x = printfn "%A" x
 
//  // these types and tokens should be moved to common module
// //////////////////////////////////////// AST DEFINITION ///////////////////////////////////////   
// type Ast =
//     | Const of Literal
//     | Var of Identifier
//     | FuncApp of Ast * Ast
//     | BuiltInFunc of BuiltInType
//     | FuncDefExp of Identifier * Ast * Ast // same as let in http://v2matveev.blogspot.com/2010/05/f-parsing-simple-language.html
//     | FuncDefExpRec of Identifier * Ast * Ast // this would be a very neat addition
//     | Lambda of Identifier * Ast
//     | Conditional of Ast * Ast * Ast 
// /////////////////////////////////////////// PARSER ////////////////////////////////////////////

// // Single Case D.U. used as a wrapper to create a type
// type Parser<'T> = P of (list<Token> -> int -> Option<'T * int>)

// // head::tail
// type PResult<'T> =
// | Success  of 'T * list<Token>
// | Failure of Failure

// // To enable computation expresion syntax
// type ParserBuilder() =
//     member this.Return(v) = one v
//     member this.Bind(p, f) = bind p f
//     member this.ReturnFrom(p) = p
//     member this.Zero() = failed ()
// let parser = ParserBuilder()

// // let pI
// let rec pExpr = 
//         let parseIdentifier = map identifier Var
//         let parseNumber = map integer Integer
