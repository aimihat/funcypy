module Parser

open Common

let print x = printfn "%A" x

/////////////////////////////////////////// PARSER ////////////////////////////////////////////

// Single Case D.U. used as a wrapper to create a type
type Parser<'T> = P of (list<Token> -> int -> Option<'T * int>)

// Basic building block *pToken*: Takes in token list and index *i*; Returns either Some 
// tuple of the token at *i* and the incremented *i* or None
let pToken: Parser<Token> =
    P <| fun tokenList i ->
        if i < tokenList.Length then Some(tokenList.[i], i + 1) else None

// Helper function: Helps to run *aParser* easily
let pRun (P aParser) tokL = aParser tokL 0

// Takes a *Token* and always returns Some tuple of *Token* and an unaltered index *i*
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

// Combines two parsers together
let pCombine (uParser: Parser<'U>) (tParser: Parser<'T>): Parser<'U> = tParser |> pBind (fun _ -> uParser)

// Applies two parsers and only keeps result of right parser
let pKeepRight uParser tParser = pCombine uParser tParser

// Applies two parsers and only keeps result of left parser
let pKeepLeft (uParser: Parser<'U>) (tParser: Parser<'T>): Parser<'T> =
    tParser |> pBind (fun tokenValue -> uParser |> pBind (fun _ -> pReturn tokenValue))

// Takes parser of a list of *tparser* and returns Some tuple of values
// and the index *i* at which the parser fails (if it does!)
// Can be used to parse sequences of tokens
let pMany (P t): Parser<'T list> =
    P <| fun tokL index ->
        // define tail recursive "loop"
        let rec loop lst i =
            match t tokL i with
            | None -> Some(List.rev lst, i)
            | Some(tokValue, tpos) -> loop (tokValue :: lst) tpos
        loop [] index // call the loop

// Similar to pMany but requires parsing success at least once
// Note: is left associative and loop is tail recursive so optimised for F#
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
        member x.Bind(t, uf) = pBind uf t // Enables let!
        member x.Combine(t, u) = pCombine u t // Enables do!
        member x.Return v = pReturn v // Enables return
        member x.ReturnFrom p: Parser<'T> = p // Enables return!
        member x.Zero() = pReturn() // allows if x then expr with no else
    end

let parser = ParserBuilder()

// Token -> bool; token type checking functions used for unpacking annotation noise
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

// Parses token and if satisfy evaluates to true then returns token, else returns fail
let pSatisfy (satisfy: Token -> bool): Parser<Token> =
    parser {
        let! tok = pToken
        if satisfy tok then return tok else return! pFail()
    }

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
        | Some(tvalue, tpos) -> Some(tvalue, tpos)

// (AND combinator) applies first parser to source stream, then applies second to remaining part of stream
let pAnd (P uParser) (P tParser) =
    P <| fun str pos ->
        match tParser str pos with
        | Some(tvalue, tpos) -> uParser tvalue tpos
        | _ -> None

// Define combinators: using static member to attach methods specifically to Parser type
// *member* keyword shows that this is a member function (i.e. a method)
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
        return head :: tail 
    }

// Skips a specific token given as input
let pSkipToken tok =
    parser {
        let! token = pToken
        if tok = token then return () else return! pFail()
    }
    
// Up to here we defined basic parsers and parser combinators and now we will work on defining the language grammar
// Get AST Type from BuiltIn Operator, Literal (Const), Identifier (Var), Special Operators
let pBuiltInFunc = pSatisfy isBuiltInOp |>> fun tok ->
    match tok with
    | TokBuiltInOp op -> op |> BuiltInFunc
    | _ -> failwith "Expected BuiltInOp but did not receive BuiltInOp"

let pConst = pSatisfy isLiteral |>> fun tok ->
    match tok with
    | TokLit(Bool x) -> (Bool x) |> Const
    | TokLit(Int x) -> (Int x) |> Const
    | TokLit(Double x) -> (Double x) |> Const
    | TokLit (String x) -> (String x) |> Const
    | TokLit(Tuple(x, y)) -> (Tuple(x, y)) |> Const
    | _ -> failwith "Expected Literal but did not receive literal"

let pVariable = pSatisfy isIdentifier |>> fun tok ->
    match tok with
    | TokIdentifier str -> str |> Var
    | _ -> failwith "Expected Identifier but did not receive Identifier"

let pSpecOp = pSatisfy isOperator |>> fun tok -> 
        match tok with
        | TokSpecOp op -> op
        | _ -> failwith "Expected Operator but did not receive Operator"

// Single term in expression is either constant or variable
let pTerm = pConst <|> pVariable

// pOp Skips the operator and builds an FuncApp AST
let pOp opTok operator =
    pSkipToken opTok |>> fun c leftTree rightTree -> FuncApp(FuncApp((BuiltInFunc operator), leftTree), rightTree)

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

// Top level chained Function application parser with operator precedence integrated
let pChainedFuncApps = pChainlMin1 pchainAddSubtract pCompOps

// Top Level AST parser: called with pRun
let rec pExpr =
    let pFuncDefExp, pFuncDefExpRec =
        let p keyword f =
            parser {
                do! pSkipToken (TokSpecOp keyword)
                let! (Var id) = pVariable
                let! value = pMany pVariable
                do! pSkipToken (TokSpecOp EQUALS)
                let! body = pExpr
                return f (id, value, body)
            }
        p LET FuncDefExp, p LETREC FuncDefExpRec

    let pBracketed =
        parser {
            do! pSkipToken (TokSpecOp LRB)
            let! e = pExpr
            do! pSkipToken (TokSpecOp RRB)
            return e
        }

    let parseLambda =
        parser {
            do! pSkipToken (TokSpecOp LAMBDA)
            let! arg = pExpr
            do! pSkipToken (TokSpecOp ARROWFUNC)
            let! body = pExpr
            return Lambda(arg, body)
        }

    let pFuncApp =
        parser {
            let! leftTree = pVariable <|> pBracketed
            let! operator = pBuiltInFunc
            let! rightList = pManyMin1 (pVariable <|> pConst <|> pBracketed)
            let rightTree = rightList |> List.reduce (fun acc e -> FuncApp(acc, e))
            return FuncApp(FuncApp(operator, leftTree), rightTree)
        }

    let pIfThenElse =
        parser {
            let pBody = pChainedFuncApps <|> pBracketed <|> pVariable <|> pConst
            do! pSkipToken (TokSpecOp IF)
            let! condition = pVariable <|> pBracketed
            do! pSkipToken (TokSpecOp THEN) 
            let! ifTrue = pBody
            do! pSkipToken (TokSpecOp ELSE)
            let! ifFalse = pBody
            return Conditional(condition, ifTrue, ifFalse)
        }

    pFuncDefExp <|> pFuncDefExpRec <|> parseLambda <|> pIfThenElse <|> pFuncApp <|> pBracketed <|> pChainedFuncApps <|> pVariable <|> pConst 