<<<<<<< HEAD
module rec Parser

// open Lexer James module
=======
module Parser
>>>>>>> 5df67b029267b4fb1f2a63a81340acdc28700090

open Common
// open Tokeniser

// The Parser code takes a list of tokens and turns this into a parse tree which can easily be evaluated.
// The process is complicated because it must cope with function application

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
    | BuiltInFunc of Arithmetic

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
let pRun (P aParser) tok = aParser tok 0

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
        // Enables return
        member x.ReturnFrom p = p : Parser<'T>
        // allows if x then expr with no else
        member x.Zero () = pReturn ()
    end

let parser = ParserBuilder ()
  
// Token -> bool; token type checking functions
let isLiteral (tok:Token) = 
    match tok with
    | TokLit _ -> true
    | _ -> false

let isUnaryOp (tok:Token) =
    match tok with
    | TokUnaryOp _ -> true
    | _ -> false

let isBinaryOp (tok:Token) =
    match tok with
    | TokBinOp _ -> true
    | _ -> false

let isStartOp (tok:Token) = 
    match tok with
    | TokStartOp _ -> true
    | _ -> false

let isEndOp (tok:Token) = 
    match tok with 
    | TokEndOp _ -> true
    | _ -> false

let isIdentifier (tok:Token) =
    match tok with
    | TokIdentifier _ -> true
    | _ -> false

let isComparisonOp (tok:Token) =
    match tok with
    | TokComparisonOp _ -> true
    | _ -> false

let isArithmeticOp (tok:Token) =
    match tok with
    | TokArithmeticOp _ -> true
    | _ -> false

// Token -> bool; token type checking functions
let getLiteral (tok:Token) = 
    match tok with
    | TokLit (Bool x) -> (Bool x)
    | TokLit (Int x) -> (Int x)
    | TokLit (Double x) -> (Double x)
    | TokLit (String x) -> (String x)
    | TokLit (Tuple (x, y)) -> (Tuple (x, y))
    | _ -> failwith "Did not get a literal" 

let getIdentifier (tok:Token) =
    match tok with
    | TokIdentifier str -> str
    | _ -> failwith "Did not get an identifier" 

let getArithmOperator (tok:Token) =
    match tok with 
    | TokArithmeticOp op -> op
    | _ -> failwith "Did not get an arithmetic operator"

////////////////////////////////////////// everything up to hear is working for tokens 
// More generic than pLiteral is pSatisfy
let pSatisfy (satisfy : Token -> bool) : Parser<Token> = 
    parser {
        let! tok = pToken
        if satisfy tok then
            return tok
        else return! pFail ()
    }

let pLiteral = pSatisfy isLiteral
let pUnaryOp = pSatisfy isUnaryOp
let pBinOp = pSatisfy isBinaryOp
let pStartOp = pSatisfy isStartOp
let pEndOp = pSatisfy isEndOp
let pIdent = pSatisfy isIdentifier
let pCompOp = pSatisfy isComparisonOp
let pArithmOp = pSatisfy isArithmeticOp
// could easily add whitespace here

// TODO: is variable name tokenType or should it be tokenValue
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

// not ready to handle equals yet

// a + b -> FuncApp(FuncApp(builtinPlus, a), b)
// current objective 
let x a b = 
    FuncApp (FuncApp ((BuiltInFunc ADD), a), b)

let pConst = pLiteral |>> getLiteral |>> Const
let pVariable = pIdent |>> getIdentifier |>> Var

let pBuiltInFunc = pArithmOp |>> getArithmOperator |>> BuiltInFunc

let pTerm = pConst <|> pVariable

// pOp
let pOp opTok operator = 
    pSkipToken opTok 
    |>> fun c -> 
        fun leftTree rightTree -> 
            FuncApp (FuncApp ((BuiltInFunc operator), leftTree), rightTree)

let pAdd = pOp (TokArithmeticOp ADD) ADD
let pSubtract = pOp (TokArithmeticOp SUBTRACT) SUBTRACT
let pMultiply = pOp (TokArithmeticOp MULTIPLY) MULTIPLY
let pDivide = pOp (TokArithmeticOp DIVIDE) DIVIDE

let pAllOp = pAdd <|> pSubtract <|> pMultiply <|> pDivide

let pmultiOrDivide = pMultiply <|> pDivide
let paddOrSubtract = pAdd <|> pSubtract

let pchainMultiDivide = pChainlMin1 pTerm pmultiOrDivide
let pchainAddSubtract = pChainlMin1 pchainMultiDivide paddOrSubtract

let pAST = pchainAddSubtract