module Parser

open Common
// open Tokeniser

// Given Examples
// builtinPlus a b -> FuncApp(FuncApp(builtinPlus, a), b)
// a + b -> FuncApp(FuncApp(builtinPlus, a), b)
// a * b -> FuncApp(FuncApp(builtinTimes, a), b)
// (a+b)*c) -> FuncApp(FuncApp(builtinTimes, FuncApp(FuncApp(builtinPlus,a),b)),c)

let print x = printfn "%A" x
 
 // these types and tokens should be moved to common module
//////////////////////////////////////// AST DEFINITION ///////////////////////////////////////   
type Ast =
    | Const of Literal
    | Var of Identifier
    | FuncApp of Ast * Ast
    | BuiltInFunc of BuiltInType
    | FuncDefExp of Identifier * Ast * Ast
    | Conditional of Ast * Ast * Ast 
/////////////////////////////////////////// PARSER ////////////////////////////////////////////

// Single Case D.U. used as a wrapper to create a type
type Parser<'T> = P of (list<Token> -> int -> Option<'T * int>)

// Basic building block *pToken*: Takes in a token list and a position in the token list index *i*
// Returns either Some tuple of the token at *i* and the incremented *i* or None
let pToken : Parser<Token> =
    P <| fun tokenList i ->
        if i < tokenList.Length then
            Some (tokenList.[i], i+1)
        else
            None

// Helper function: Helps to run *aParser* easily
let pRun (P aParser) tokL = aParser tokL 0

// Takes a *Token* and always returns Some tuple of *Token* and an unaltered index *i*
// More generic types used to parse specific Token Types
let pReturn tok : Parser<'T> = 
    P <| fun t i -> Some(tok, i)

// Takes unit and always returns None
let pFail () : Parser<'T> = 
    P <| fun t i -> None

// Following standard functional pattern, takes output of one parser and feeds it
// as input to another parser. This allows the chaining of parsers together.
// *ufunc* is a function that takes a type T and returns a parser of some type U
// *tparser* is a parser of the same type U
let pBind (ufunc : 'T -> Parser<'U>) (P tparser) : Parser<'U> =
    P <| fun tokenList i ->
        match tparser tokenList i with
        | None -> None
        | Some (tvalue, newI) ->
            let (P uparser) = ufunc tvalue
            uparser tokenList newI 

// Given two parsers *uParser* and *tParser* combine them using *pbind* as follows:
// Combines two parsers together
let pCombine (uParser : Parser<'U>) (tParser : Parser<'T>) : Parser<'U> = 
    tParser |> pBind (fun _ -> uParser)

// Applies two parsers and only keeps result of right parser
let pKeepRight uParser tParser = 
    pCombine uParser tParser

// Applies two parsers and only keeps result of left parser
let pKeepLeft (uParser : Parser<'U>) (tParser : Parser<'T>) : Parser<'T> = 
  tParser |> pBind (fun tokenValue -> uParser |> pBind (fun _ -> pReturn tokenValue))

// Takes a parser of a list of *tparser* and returns Some tuple of values
// and the index *i* at which the parser fails (if it does!)
// Can be used to parse sequences of tokens
let pMany (P t) : Parser<'T list> =
    P <| fun tokL pos ->
        // define a tail recursive "loop" in a functional way
        let rec loop vs currentPos = 
            match t tokL currentPos with
            | None -> Some (List.rev vs, currentPos)
            | Some (tvalue, tpos) -> loop (tvalue::vs) tpos
        loop [] pos // call the loop

// Similar to pMany but requires parsing success at least once
// Note this is left associative
// loop is tail recursive so optimised for F#
let pChainlMin1 (term : Parser<'T>) (sep : Parser<'T -> 'T -> 'T>) : Parser<'T> =
    let (P termfun) = term
    let (P sepfun) = sep
    P <| fun tok i ->
        let rec loop aggr currentI =
            match sepfun tok currentI with
            | None -> Some (aggr, currentI)
            | Some (sepCombiner, sepI) ->
                match termfun tok sepI with
                | None -> None
                | Some (termValue, termI) -> loop (sepCombiner aggr termValue) termI
        match termfun tok i with
        | None -> None
        | Some (termValue, termI) -> loop termValue termI

// F# Computation expression: makes it easier to build more complex parsers
// Standard FP pattern using earlier defined building block functions
type ParserBuilder () =
    class 
        // Enables let!
        member x.Bind (t, uf) = pBind uf t
        // Enables do!
        member x.Combine (t, u) = pCombine u t
        // Enables return
        member x.Return v = pReturn v
        // Enables return!
        member x.ReturnFrom p = p : Parser<'T>
        // allows if x then expr with no else
        member x.Zero () = pReturn ()
    end

let parser = ParserBuilder ()
  
// Token -> bool; token type checking functions -> also sort of wasteful -> a lot of unpacking
let isLiteral (tok:Token) = 
    match tok with
    | TokLit _ -> true
    | _ -> false

let isUnaryOp (tok:Token) =
    match tok with
    | TokUnaryOp _ -> true
    | _ -> false

let isOperator (tok:Token) = 
    match tok with
    | TokSpecOp _ -> true
    | _ -> false

let isIdentifier (tok:Token) =
    match tok with
    | TokIdentifier _ -> true
    | _ -> false

let isBuiltInOp (tok:Token) =
    match tok with
    | TokBuiltInOp _ -> true
    | _ -> false

// Token -> bool; token type checking functions -> not a fan...these are very repetitive
let getLiteral (tok:Token) = 
    match tok with
    | TokLit (Bool x) -> (Bool x)
    | TokLit (Int x) -> (Int x)
    | TokLit (Double x) -> (Double x)
    // | TokLit (String x) -> (String x)
    | TokLit (Tuple (x, y)) -> (Tuple (x, y))
    | _ -> failwith "Expected Literal but did not receive literal" 

let getIdentifier (tok:Token) =
    match tok with
    | TokIdentifier str -> str
    | _ -> failwith "Expected Identifier but did not receive Identifier" 

let getBuiltInOp (tok:Token) =
    match tok with 
    | TokBuiltInOp op -> op
    | _ -> failwith "Expected BuiltInOp but did not receive BuiltInOp"
   
let getOperator (tok:Token) =
    match tok with
    | TokSpecOp op -> op
    | _ -> failwith "Expected Operator but did not receive Operator"

let pSatisfy (satisfy : Token -> bool) : Parser<Token> = 
    parser {
        let! tok = pToken
        if satisfy tok then
            return tok
        else return! pFail ()
    }

let pLiteral = pSatisfy isLiteral
let pUnaryOp = pSatisfy isUnaryOp
let pStartOp = pSatisfy isOperator
let pIdent = pSatisfy isIdentifier
let pIdentifier = pSatisfy isIdentifier
// could easily add whitespace here

// Takes a mapping function that maps a type T to type U and a parser of T
let pMap mappingFunc tParser =
    parser {
        let! tokenType = tParser
        return mappingFunc tokenType
    }

// Combines two parsers into a Parser of a Pair
let pPair uParser tParser =
    parser {
        let! first = tParser
        let! second = uParser
        return first, second
    }

// Combines two parsers such that if uParser fails it tries tParser
let pOrElse (P uParser) (P tParser) =
    P <| fun str pos ->
        match tParser str pos with
        | None -> uParser str pos
        | Some (tvalue, tpos) -> Some (tvalue, tpos)

// Define combinators: using static member to attach methods specifically to Parser type
// *member* keyword shows that this is a member function (i.e. a method)
// Technically OOP approach but cleaner and combinators only needed specifically for Parsers here
// After this we can express parsers using combinators to make things even more readable!
type Parser<'T> with
    static member (>>=) (t, uf) = pBind uf t
    static member (>>.) (t, u) = pKeepRight u t
    static member (.>>) (t, u) = pKeepLeft u t
    static member (.>>.) (t, u) = pPair u t
    static member (|>>) (t, m) = pMap m t
    static member (<|>) (t, u) = pOrElse u t

// Similar to pMany but requires 1 or more 'T instead of 0 or more
let pManyMin1 tparser = 
    parser {
        let! head = tparser
        let! tail = pMany tparser
        return head::tail
    }

// Skips a specific token given as input
let pSkipToken tok =
    parser {
        let! token = pToken
        if tok = token then   
            return ()
        else
            return! pFail ()
    }

let pConst = pLiteral |>> getLiteral |>> Const
let pVariable = pIdent |>> getIdentifier |>> Var
let pBuiltInFunc = pSatisfy isBuiltInOp |>> getBuiltInOp |>> BuiltInFunc
let pTerm = pConst <|> pVariable

// pOp Skips the operator and builds an AST with the operator in the right place
let pOp opTok operator = 
    pSkipToken opTok 
    |>> fun c -> 
        fun leftTree rightTree -> 
            FuncApp (FuncApp ((BuiltInFunc operator), leftTree), rightTree)

let pAdd = pOp (TokBuiltInOp ADD) ADD
let pSubtract = pOp (TokBuiltInOp SUBTRACT) SUBTRACT
let pMultiply = pOp (TokBuiltInOp MULTIPLY) MULTIPLY
let pDivide = pOp (TokBuiltInOp DIVIDE) DIVIDE

// let pAllOp = pAdd <|> pSubtract <|> pMultiply <|> pDivide

let pmultiOrDivide = pMultiply <|> pDivide
let paddOrSubtract = pAdd <|> pSubtract

let pchainMultiDivide = pChainlMin1 pTerm pmultiOrDivide
let pchainAddSubtract = pChainlMin1 pchainMultiDivide paddOrSubtract
  
// can you use reduce on this combinator -> what style indentation is "readable here"
// is this better than a match statement?
// unwrap and get rid of the list by choosing the zeroth element

// Conditional of Ast * Ast * Ast
// Conditional(condition<Ast>, ifTrue<Ast>, ifFalse<Ast>)
(*
    
(Conditional(FuncApp (FuncApp ((BuiltInFunc LE), Const (Int 6)), Const (Int 10)),
                Const (Int 5),
                    Const (Int 10)), 8)
// ((([Const (Int 6); BuiltInFunc LE; Const (Int 10)], [Const (Int 5)]),[Const (Int 10)]), 8)
*)

let pIf = 
    pSkipToken (TokSpecOp IF)
    >>. pMany (pTerm <|> pBuiltInFunc) 
    .>> pSkipToken (TokSpecOp THEN) 
    .>>. pMany (pTerm <|> pBuiltInFunc) 


let pCond = pSkipToken (TokSpecOp IF) >>. 
            pMany (pTerm <|> pBuiltInFunc) .>> 
            pSkipToken (TokSpecOp THEN) .>>. 
            pMany (pTerm <|> pBuiltInFunc) .>> 
            pSkipToken (TokSpecOp ELSE) .>>. 
            pMany (pTerm <|> pBuiltInFunc)
            // |>> fun c ->
            //     fun condition ifTrue ifFalse ->
            //         Conditional(condition, ifTrue, ifFalse)
 
// if a then b else c

let pAST = pCond
////////////////////////////////////////////////////////////////////////////////// 
// current objective 
// <defn-exp> ::= "let" <var-list> "=" <exp> "in" <exp> "ni"
// <if-exp> ::= "if" <exp> "then" <exp> "else" <exp> "fi"
// let str = "let f x = x x"

// Un-used code
// let pCond = 
//     parser {
//         do! pSkipToken (TokStartOp IF)
//         let! condition = pMany pchainAddSubtract
//         do! pSkipToken (TokEndOp THEN)
//         let! ifTrue = pMany pchainAddSubtract
//         do! pSkipToken (TokEndOp ELSE)
//         let! ifFalse = pMany pToken
//         return condition, ifTrue, ifFalse
//     }

// let pFuncDefExp = 
//     pSkipToken (TokStartOp FUN)
//     |>> fun c ->
//         fun funcName leftTree rightTree ->
//             FuncDefExp (funcName, (pMany pAST)(leftTree, rightTree))

// FuncDefExp ((Identifier funcName), _, _)

// STARTOP let
// f of Identifier
// arg of identifier 
// EQUALS 
// let x a b = 
//     FuncApp (FuncApp ((BuiltInFunc ADD), a), b)

// problem is that it cannot collapse ifTrue and ifFalse into independent asts
// let pConditional = 
//     printf "entered pCond"
//     pSkipToken (TokStartOp IF)
//     |>> fun c ->
//         printf "anonym func 1"
//         fun condition arg1 ifTrue arg2 ifFalse -> 
//             printf "entered anonym func 2"
//             match arg1 with 
//             | THEN ->
//                 match arg2 with
//                 | ELSE -> Conditional(condition, ifTrue, ifFalse)
//                 | _ -> failwith "expected an else here"
//             | _ -> failwith "expected a then here"

// conditionals
// function definition expression: fun x = x + 1
// multiple inputs
// recursive bracketing

(*
    x = 1
    y = 2
    z = x + y
*)