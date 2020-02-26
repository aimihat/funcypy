module Parser

open Common

let print x = printfn "%A" x

// these types and tokens should be moved to common module
//////////////////////////////////////// AST DEFINITION ///////////////////////////////////////
type Ast =
    | Const of Literal
    | Var of Identifier
    | FuncApp of Ast * Ast
    | BuiltInFunc of BuiltInType
    | FuncDefExp of Identifier * Ast * Ast // same as let in http://v2matveev.blogspot.com/2010/05/f-parsing-simple-language.html
    | FuncDefExpRec of Identifier * Ast * Ast // this would be a very neat addition
    | Lambda of Ast * Ast
    | Conditional of Ast * Ast * Ast
/////////////////////////////////////////// PARSER ////////////////////////////////////////////

// Single Case D.U. used as a wrapper to create a type
type Parser<'T> = P of (list<Token> -> int -> Option<'T * int>)

// Basic building block *pToken*: Takes in a token list and a position in the token list index *i*
// Returns either Some tuple of the token at *i* and the incremented *i* or None
let pToken: Parser<Token> =
    P <| fun tokenList i ->
        if i < tokenList.Length then Some(tokenList.[i], i + 1) else None

// Helper function: Helps to run *aParser* easily
let pRun (P aParser) tokL = aParser tokL 0

// Takes a *Token* and always returns Some tuple of *Token* and an unaltered index *i*
// More generic types used to parse specific Token Types
let pReturn tok: Parser<'T> = P <| fun t i -> Some(tok, i)

// Takes unit and always returns None
let pFail(): Parser<'T> = P <| fun t i -> None

// Following standard functional pattern, takes output of one parser and feeds it
// as input to another parser. This allows the chaining of parsers together.
// *ufunc* is a function that takes a type T and returns a parser of some type U
// *tparser* is a parser of the same type U
let pBind (ufunc: 'T -> Parser<'U>) (P tparser): Parser<'U> =
    P <| fun tokenList i ->
        match tparser tokenList i with
        | None -> None
        | Some(tvalue, newI) ->
            let (P uparser) = ufunc tvalue
            uparser tokenList newI

// Given two parsers *uParser* and *tParser* combine them using *pbind* as follows:
// Combines two parsers together
let pCombine (uParser: Parser<'U>) (tParser: Parser<'T>): Parser<'U> = tParser |> pBind (fun _ -> uParser)

// Applies two parsers and only keeps result of right parser
let pKeepRight uParser tParser = pCombine uParser tParser

// Applies two parsers and only keeps result of left parser
let pKeepLeft (uParser: Parser<'U>) (tParser: Parser<'T>): Parser<'T> =
    tParser |> pBind (fun tokenValue -> uParser |> pBind (fun _ -> pReturn tokenValue))

// Takes a parser of a list of *tparser* and returns Some tuple of values
// and the index *i* at which the parser fails (if it does!)
// Can be used to parse sequences of tokens
let pMany (P t): Parser<'T list> =
    P <| fun tokL pos ->
        // define a tail recursive "loop" in a functional way
        let rec loop vs currentPos =
            match t tokL currentPos with
            | None -> Some(List.rev vs, currentPos)
            | Some(tvalue, tpos) -> loop (tvalue :: vs) tpos
        loop [] pos // call the loop

// Similar to pMany but requires parsing success at least once
// Note this is left associative
// loop is tail recursive so optimised for F#
let pChainlMin1 (term: Parser<'T>) (sep: Parser<'T -> 'T -> 'T>): Parser<'T> =
    let (P termfun) = term
    let (P sepfun) = sep
    P <| fun tok i ->
        let rec loop aggr currentI =
            match sepfun tok currentI with
            | None -> Some(aggr, currentI)
            | Some(sepCombiner, sepI) ->
                match termfun tok sepI with
                | None -> None
                | Some(termValue, termI) -> loop (sepCombiner aggr termValue) termI
        match termfun tok i with
        | None -> None
        | Some(termValue, termI) -> loop termValue termI

// F# Computation expression: makes it easier to build more complex parsers
// Standard FP pattern using earlier defined building block functions
type ParserBuilder() =
    class
        // Enables let!
        member x.Bind(t, uf) = pBind uf t
        // Enables do!
        member x.Combine(t, u) = pCombine u t
        // Enables return
        member x.Return v = pReturn v
        // Enables return!
        member x.ReturnFrom p: Parser<'T> = p
        // allows if x then expr with no else
        member x.Zero() = pReturn()
    end

let parser = ParserBuilder()

// Token -> bool; token type checking functions -> also sort of wasteful -> a lot of unpacking
let isLiteral (tok: Token) =
    match tok with
    | TokLit _ -> true
    | _ -> false

let isUnaryOp (tok: Token) =
    match tok with
    | TokUnaryOp _ -> true
    | _ -> false

let isOperator (tok: Token) =
    match tok with
    | TokSpecOp _ -> true
    | _ -> false

let isIdentifier (tok: Token) =
    match tok with
    | TokIdentifier _ -> true
    | _ -> false

let isBuiltInOp (tok: Token) =
    match tok with
    | TokBuiltInOp _ -> true
    | _ -> false

let isWhiteSpace (tok: Token) =
    match tok with
    | TokWhitespace _ -> true
    | _ -> false

// Token -> bool; token type checking functions -> not a fan...these are very repetitive
let getLiteral (tok: Token) =
    match tok with
    | TokLit(Bool x) -> (Bool x)
    | TokLit(Int x) -> (Int x)
    | TokLit(Double x) -> (Double x)
    // | TokLit (String x) -> (String x)
    | TokLit(Tuple(x, y)) -> (Tuple(x, y))
    | _ -> failwith "Expected Literal but did not receive literal"

let getIdentifier (tok: Token) =
    match tok with
    | TokIdentifier str -> str
    | _ -> failwith "Expected Identifier but did not receive Identifier"

let getBuiltInOp (tok: Token) =
    match tok with
    | TokBuiltInOp op -> op
    | _ -> failwith "Expected BuiltInOp but did not receive BuiltInOp"

let getOperator (tok: Token) =
    match tok with
    | TokSpecOp op -> op
    | _ -> failwith "Expected Operator but did not receive Operator"

let getWhiteSpace (tok: Token) =
    match tok with
    | TokWhitespace op -> op
    | _ -> failwith "Expected white space but did not receive white space"

let pSatisfy (satisfy: Token -> bool): Parser<Token> =
    parser {
        let! tok = pToken
        if satisfy tok then return tok else return! pFail()
    }

let pLiteral = pSatisfy isLiteral
let pUnaryOp = pSatisfy isUnaryOp
let pStartOp = pSatisfy isOperator
let pIdentifier = pSatisfy isIdentifier
let pWhitespace = pSatisfy isWhiteSpace

// Takes a mapping function that maps a type T to type U and a parser of T
let pMap mappingFunc tParser =
    parser {
        let! tokenType = tParser
        return mappingFunc tokenType }

// Combines two parsers into a Parser of a Pair
let pPair uParser tParser =
    parser {
        let! first = tParser
        let! second = uParser
        return first, second }

// Combines two parsers such that if uParser fails it tries tParser
let pOrElse (P uParser) (P tParser) =
    P <| fun str pos ->
        match tParser str pos with
        | None -> uParser str pos
        | Some(tvalue, tpos) -> Some(tvalue, tpos)

// (AND combinator) applies first parser to source stream, then applies second to remaining part of stream
let pAnd (P uParser) (P tParser) =
    P <| fun str pos ->
        match tParser str pos with
        | Some(tvalue, tpos) -> uParser tvalue tpos
        | _ -> None

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
    static member (<&>) (t, u) = pAnd u t

// Similar to pMany but requires 1 or more 'T instead of 0 or more
let pManyMin1 tparser =
    parser {
        let! head = tparser
        let! tail = pMany tparser
        return head :: tail }

// Skips a specific token given as input

// THE PROBLEM IS HERE!!!
let pSkipToken tok =
    parser {
        let! token = pToken
        if tok = token then return () else return! pFail()
    }
    
// Up to here we defined basic parsers and parser combinators and now we will work on defining the language grammar
// Get AST Type from BuiltIn Operator
let pBuiltInFunc = pSatisfy isBuiltInOp |>> getBuiltInOp |>> BuiltInFunc
// Get AST Type from Literal (Const)
let pConst = pLiteral |>> getLiteral |>> Const
// Get AST Type from Identifier, which is used to identify variables (Var)
let pVariable = pIdentifier |>> getIdentifier |>> Var
// a single term in an expression is either a constant or a variable
let pTerm = pConst <|> pVariable

// pOp Skips the operator and builds an AST with the operator in the right place
let pOp opTok operator =
    pSkipToken opTok |>> fun c leftTree rightTree -> FuncApp(FuncApp((BuiltInFunc operator), leftTree), rightTree)

// x+1*(3+2)

let pSpecOp = pSatisfy isOperator |>> getOperator

// Define parser for BuiltInFuncTypes
// add left right

let pAdd = pOp (TokBuiltInOp ADD) ADD
let pSubtract = pOp (TokBuiltInOp SUBTRACT) SUBTRACT
let pMultiply = pOp (TokBuiltInOp MULTIPLY) MULTIPLY
let pDivide = pOp (TokBuiltInOp DIVIDE) DIVIDE

let pLessThan = pOp (TokBuiltInOp LT) LT
let pLessThanOrEq = pOp (TokBuiltInOp LE) LE
let pGreaterThan = pOp (TokBuiltInOp GT) GT
let pGreaterThanOrEq = pOp (TokBuiltInOp GE) GE
let pEqualTo = pOp (TokBuiltInOp EQ) EQ

// Define precedence of basic BuiltInType operators
let pAllOp = pAdd <|> pSubtract <|> pMultiply <|> pDivide // not currently being used
let pmultiOrDivide = pMultiply <|> pDivide
let paddOrSubtract = pAdd <|> pSubtract
let pchainMultiDivide = pChainlMin1 pTerm pmultiOrDivide
let pchainAddSubtract = pChainlMin1 pchainMultiDivide paddOrSubtract

let pCompOps = pLessThan <|> pLessThanOrEq <|> pGreaterThan <|> pGreaterThanOrEq <|> pEqualTo
let pChainCompOps = pChainlMin1 pTerm pCompOps

let pChainFuncApps = pChainlMin1 pchainAddSubtract pCompOps

let var = pMany (pSkipToken (TokWhitespace(Space)))

let ignoreList =
    let reducer list =
        match list with
        | [] -> ()
        | lst -> lst |> List.reduce (fun a b -> ())
    pMap reducer

let pSkipToken' tok = pMany (pSkipToken (tok)) |> ignoreList
let pSkipTokenMin1' tok = pManyMin1 (pSkipToken (tok)) |> ignoreList

let rec pExpr =
    let pFuncDefExp, pFuncDefExpRec =
        let p keyword f =
            parser {
                do print "entered funcdefexp"
                do! pSkipToken (TokSpecOp keyword)
                do! pSkipToken' (TokWhitespace(Space))
                let! (Var id) = pVariable
                do printf "\n id: %A \n" id
                do! pSkipToken (TokSpecOp EQUALS) // passes this point successfully
                do print "found equals"
                let! value = pExpr
                do print "found expression??"
                do printf "\n value: %A" value
                do! pSkipToken (TokSpecOp IN)
                do! pSkipToken' (TokWhitespace(Space))
                let! body = pExpr
                do printf "\n body: %A" body
                return f (id, value, body)
            }
        p LET FuncDefExp, p LETREC FuncDefExpRec

    let parseLambda =
        parser {
            do print "entered lambda"
            do! pSkipToken (TokSpecOp LAMBDA)
            do! pSkipToken' (TokWhitespace(Space))
            let! arg = pExpr
            do! pSkipToken (TokSpecOp ARROWFUNC)
            let! body = pExpr
            return Lambda(arg, body)
        }

    // This is working yay
    let pBracketed =
        parser {
            do print "entered bracket"
            do! pSkipToken' (TokWhitespace(Space))
            do print "between whitespace and lrb"
            do! pSkipToken (TokSpecOp LRB)
            do print "between whitespace and brackets"
            let! e = pExpr
            do printf "after e %A" e
            do! pSkipToken (TokSpecOp RRB)
            do print "after rrb bracket"
            return e
        }

    let pFuncApp =
        parser {
            do print "entered funcapp"
            let! leftTree = pVariable <|> pBracketed
            do printf "(%A)\n" leftTree
            let! operator = pBuiltInFunc
            do printf "%A" operator
            let! rightList = pManyMin1 (pVariable <|> pConst <|> pBracketed)
            do printf "%A" rightList
            let rightTree = rightList |> List.reduce (fun acc e -> FuncApp(acc, e))
            do printf "%A" rightTree
            return FuncApp(FuncApp(operator, leftTree), rightTree)
        }

    let pIfThenElse =
        parser {
            do print "entered ifthenelse"
            let body = pConst <|> pVariable <|> pBracketed <|> pConst
            do! pSkipToken' (TokWhitespace(Space))
            do! pSkipToken (TokSpecOp IF)
            let! condition = pVariable <|> pBracketed
            do printf "\n condition: %A" condition // we are getting this condition well now
            do! pSkipToken' (TokWhitespace(Space)) // changed skip token type here from pSkipTokenMin1'
            do! pSkipToken (TokSpecOp THEN) 
            do print "\nfound then" // we are also getting here
            let! ifTrue = body
            do printf "\n ifTrue: %A" ifTrue
            do! pSkipToken' (TokWhitespace(Space)) // changed skip token type here from pSkipTokenMin1'
            do! pSkipToken (TokSpecOp ELSE)
            let! ifFalse = body
            do printf "\n ifTrue: %A" ifFalse
            return Conditional(condition, ifTrue, ifFalse)
        }

    pFuncDefExp <|> pFuncDefExpRec <|> parseLambda <|> pIfThenElse <|> pFuncApp <|> pBracketed <|> pChainFuncApps <|> pVariable <|> pConst
    // pFuncDefExp <|> pFuncDefExpRec <|> parseLambda <|> pIfThenElse <|> pFuncApp <|> pBracketed <|> pVariable <|> pConst

// pexpr -> ifthenelse -> pbracketed -> pexpr -> funcapp ->
// TODO: multiple lines

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// let pAST = pExpr
//////////////////////////////////////////////////////////////////////////////////
// current objective
// <defn-exp> ::= "let" <var-list> "=" <exp> "in" <exp> "ni"
// <if-exp> ::= "if" <exp> "then" <exp> "else" <exp> "fi"
// let str = "let f x = x x"

// FuncDefExp ((Identifier funcName), _, _)

// STARTOP let
// f of Identifier
// arg of identifier
// EQUALS
// let x a b =
//     FuncApp (FuncApp ((BuiltInFunc ADD), a), b)

// conditionals
// function definition expression: fun x = x + 1
// multiple inputs
// recursive bracketing

// Conditional of Ast * Ast * Ast
// Conditional(condition<Ast>, ifTrue<Ast>, ifFalse<Ast>)

// Parser that consumes 0+ whitespaces + specified Parser
// Make sure this works before moving on
// TODO:have to make this more generic for all white spaces
// let pWs0Plus p:Parser<Token> = (pMany (pSkipToken (TokWhitespace (Space))) <&> p

// Make sure this works before moving on
// not sure about the and combinator that should be here...
// Parser that consumes 1+ whitespaces + specified Parser
// let pWs1Plus p:Parser<Token> = pWs0Plus p

// let pWs0Token tok:Token = pWs0Plus tok

// let pWs1Token tok:Token = pWs1Plus tok

// Apply is equivalent to my FuncApp

    // let pOp opTok operator =
    //     pSkipToken opTok
    //     |>> fun c ->
    //         fun leftTree rightTree ->
    //             FuncApp (FuncApp ((BuiltInFunc operator), leftTree), rightTree)
    // var or const, operator, exp

// open Tokeniser

// Given Examples
// builtinPlus a b -> FuncApp(FuncApp(builtinPlus, a), b)
// a + b -> FuncApp(FuncApp(builtinPlus, a), b)
// a * b -> FuncApp(FuncApp(builtinTimes, a), b)
// (a+b)*c) -> FuncApp(FuncApp(builtinTimes, FuncApp(FuncApp(builtinPlus,a),b)),c)