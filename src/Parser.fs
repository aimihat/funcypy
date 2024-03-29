module Parser

open Expecto.CSharp

open Helpers
/////////////////////////////////////////// PARSER ////////////////////////////////////////////

/// Single Case D.U. used as a wrapper to create a type
type Parser<'T> = P of (list<Token> -> int -> Option<'T * int>)

/// Basic building block *pToken*: Takes in token list and index *i*; Returns either Some 
/// tuple of the token at *i* and the incremented *i* or None
let pToken: Parser<Token> =
    P <| fun tokenList i ->
        if i < tokenList.Length then Some(tokenList.[i], i + 1) else None

/// Helper function: Helps to run *aParser* easily
let pRun (P aParser) tokL = aParser tokL 0

/// Takes a *Token* and always returns Some tuple of *Token* and an unaltered index *i*
let pReturn tok: Parser<'T> = P <| fun t i -> Some(tok, i)

/// Takes unit and always returns None
let pFail(): Parser<'T> = P <| fun t i -> None

/// Following standard functional pattern, takes output of one parser and feeds it
/// as input to another parser. This allows the chaining of parsers together.
/// *ufunc* is a function that takes a type T and returns a parser of some type U
/// *tparser* is a parser of the same type U
let pBind (ufunc: 'T -> Parser<'U>) (P tparser): Parser<'U> =
    P <| fun tokenList i ->
        match tparser tokenList i with
        | None -> None
        | Some(tvalue, newI) ->
            let (P uparser) = ufunc tvalue
            uparser tokenList newI

/// Combines two parsers together
let pCombine (uParser: Parser<'U>) (tParser: Parser<'T>): Parser<'U> = tParser |> pBind (fun _ -> uParser)

/// Applies two parsers and only keeps result of right parser
let pKeepRight uParser tParser = pCombine uParser tParser

/// Applies two parsers and only keeps result of left parser
let pKeepLeft (uParser: Parser<'U>) (tParser: Parser<'T>): Parser<'T> =
    tParser |> pBind (fun tokenValue -> uParser |> pBind (fun _ -> pReturn tokenValue))

/// Takes parser of a list of *tparser* and returns Some tuple of values
/// and the index *i* at which the parser fails (if it does!)
/// Can be used to parse sequences of tokens
let pMany (P t): Parser<'T list> =
    P <| fun tokL index ->
        // define tail recursive "loop"
        let rec loop lst i =
            match t tokL i with
            | None -> Some(List.rev lst, i)
            | Some(tokValue, tpos) -> loop (tokValue :: lst) tpos
        loop [] index // call the loop

/// Similar to pMany but requires parsing success at least once
/// Note: is left associative and loop is tail recursive so optimised for F#
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

/// F# Computation expression: makes it easier to build more complex parsers
/// Standard FP pattern using earlier defined building block functions
type ParserBuilder() =
    class
        member x.Bind(t, uf) = pBind uf t // Enables let!
        member x.Combine(t, u) = pCombine u t // Enables do!
        member x.Return v = pReturn v // Enables return
        member x.ReturnFrom p: Parser<'T> = p // Enables return!
        member x.Zero() = pReturn() // allows if x then expr with no else
    end

let parser = ParserBuilder()

/// Token -> bool; token type checking functions used for unpacking annotation noise
let isLiteral (tok: Token) =
    match tok with
    | TokLit _ -> true
    | _ -> false

let isIdentifier (tok: Token) =
    match tok with
    | TokIdentifier _ -> true
    | _ -> false

/// Parses token and if satisfy evaluates to true then returns token, else returns fail
let pSatisfy (satisfy: Token -> bool): Parser<Token> =
    parser {
        let! tok = pToken
        if satisfy tok then return tok else return! pFail()
    }

/// Takes a mapping function that maps a type T to type U and a parser of T
let pMap mappingFunc tParser =
    parser {
        let! tokenType = tParser
        return mappingFunc tokenType     
    }

/// Combines two parsers into a Parser of a Pair
let pPair uParser tParser =
    parser {
        let! first = tParser
        let! second = uParser
        return first, second 
    }

/// Combines two parsers such that if uParser fails it tries tParser
let pOrElse (P uParser) (P tParser) =
    P <| fun str pos ->
        match tParser str pos with
        | None -> uParser str pos
        | Some(tvalue, tpos) -> Some(tvalue, tpos)

/// (AND combinator) applies first parser to source stream, then applies second to remaining part of stream
let pAnd (P uParser) (P tParser) =
    P <| fun str pos ->
        match tParser str pos with
        | Some(tvalue, tpos) -> uParser tvalue tpos
        | _ -> None

/// Define combinators: using static member to attach methods specifically to Parser type
/// *member* keyword shows that this is a member function (i.e. a method)
/// After this we can express parsers using combinators to make things even more readable!
type Parser<'T> with
    static member (>>=) (t, uf) = pBind uf t
    static member (>>.) (t, u) = pKeepRight u t
    static member (.>>) (t, u) = pKeepLeft u t
    static member (.>>.) (t, u) = pPair u t
    static member (|>>) (t, m) = pMap m t
    static member (<|>) (t, u) = pOrElse u t
    static member (<&>) (t, u) = pAnd u t

/// Similar to pMany but requires 1 or more 'T instead of 0 or more
let pManyMin1 tparser =
    parser {
        let! head = tparser
        let! tail = pMany tparser
        return head::tail
    }

/// Skips a specific token given as input
let pSkipToken tok =
    parser {
        let! token = pToken
        if tok = token then return () else return! pFail()
    }

/// Like pSkipToken but fails if the token is missing with specific error message
let pSkipTokenOrFail tok = 
    parser {
        let! token = pToken
        if tok = token then return () else return! failwithf "missing %A" tok 
    }    

/// Get AST Type from Literal (Const), Identifier (Var)
let pConst = pSatisfy isLiteral |>> fun tok ->
    match tok with
    | TokLit(Bool x) -> (Bool x) |> Literal
    | TokLit(Int x) -> (Int x) |> Literal
    | TokLit(Double x) -> (Double x) |> Literal
    | TokLit (String x) -> (String x) |> Literal
    | _ -> failwith "Expected Literal but did not receive literal"

let pVariable = pSatisfy isIdentifier |>> fun tok ->
    match tok with
    | TokIdentifier str -> str |> Variable
    | _ -> failwith "Expected Identifier but did not receive Identifier"

let ignoreList =
    let reducer list =
        match list with
        | [] -> ()
        | lst -> lst |> List.reduce (fun a b -> ())
    pMap reducer

let combineLambdas args body =
    let rec addLambda lambdas definition =
        match lambdas with
        | [] -> definition
        | (Variable hd)::tl -> addLambda tl (Lambda(hd, definition))
        | _ -> failwithf "Shouldn't happen"
    addLambda (List.rev args) body

let combineCalls left right =
    let rec addArgs l r =
        match r with
        | [] -> l
        | hd::tl -> addArgs (DCall(l, hd)) tl
    addArgs left right

let combinePairs left right =
    let rec addPair l r =
        match r with
        | [] -> DPair(l, Null)
        | hd::tl -> DPair(l, addPair hd tl)
    addPair left right

/// Top Level AST expression parser
let rec pAst: Parser<Ast> =
    let pFuncDefExp =
        parser {
            do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            do! pSkipToken (TokSpecOp DEF)
            let! (Variable name) = pVariable
            let! arguments = pMany pVariable
            do! pSkipTokenOrFail (TokSpecOp COLON)
            do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            let! body = pAst
            do! pManyMin1 (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            let! expr = pAst 
            let definition = combineLambdas arguments body
            return FuncDefExp(name, definition, expr)
        }
         
    let pBracketed =
        parser {
            do! pSkipToken (TokSpecOp LRB)
            let! e = pAst
            do! pSkipTokenOrFail (TokSpecOp RRB)
            return e
        }

    let pLambda =
        parser {
            do! pSkipToken (TokSpecOp LAMBDA)
            let! (Variable id) = pVariable
            do! pSkipTokenOrFail (TokSpecOp COLON)
            let! body = pAst
            return Lambda(id, body)
        }

    let pNextPair = 
        parser {
            do! pSkipToken (TokSpecOp COMMA) 
            let! nextTerm = pAst
            return nextTerm
        }

    // parse full pair
    let pFullPair =
        parser {
            do! pSkipToken (TokSpecOp LSB)
            let! leftArg = pAst
            let! rightArg = pManyMin1 pNextPair
            do! pSkipTokenOrFail (TokSpecOp RSB)
            let list = combinePairs leftArg rightArg
            return list
        }

    // parse single pair
    let pHalfPair =
        parser {
            do! pSkipToken (TokSpecOp LSB)
            let! arg = pAst
            do! pSkipTokenOrFail (TokSpecOp RSB)
            return DPair(arg, Null)
        }

    // parse empty pair
    let pEmptyPair =
        parser {
            do! pSkipToken (TokSpecOp LSB)
            do! pSkipToken (TokSpecOp RSB)
            return DPair(Null, Null)
        }

    let pListFunctionApp = 
        parser {
            let pListOp opTok operator =
                pSkipToken opTok |>> fun c -> BuiltInFunc operator
            let pIsList = pListOp (TokBuiltInOp (ListF IsList)) (ListF IsList)
            let pIsEmpty = pListOp (TokBuiltInOp (ListF IsEmpty)) (ListF IsEmpty)
            let pHead = pListOp (TokBuiltInOp (ListF Head)) (ListF Head)
            let pTail = pListOp (TokBuiltInOp (ListF Tail)) (ListF Tail)
            let pImplode = pListOp (TokBuiltInOp (ListF ImplodeStr)) (ListF ImplodeStr)
            let pExplode = pListOp (TokBuiltInOp (ListF ExplodeStr)) (ListF ExplodeStr)
            
            let! listOperator = pIsList <|> pIsEmpty <|> pHead <|> pTail <|> pImplode <|> pExplode  
            let! listTerm = pConst <|> pVariable <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pBracketed
            return DCall(listOperator, listTerm)
        }

    let pListAppend = 
        parser {
            do! pSkipToken (TokBuiltInOp (ListF Append))
            let! listTerm = pVariable <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pBracketed
            let! element = pConst <|> pVariable <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pBracketed
            return DCall(DCall(BuiltInFunc (ListF Append), listTerm), element)
        }
    let pListInsert = 
        parser {
            do! pSkipToken (TokBuiltInOp (ListF Insert))
            let! listTerm = pVariable <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pBracketed
            let! element = pConst <|> pVariable <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pBracketed

            return DCall(DCall(BuiltInFunc (ListF Insert), listTerm), element)
        }

    let pOperatorApp =
        parser {
            // pOp Skips the operator and builds an OperatorApp AST (operators applied to 2 arguments)
            let pOp opTok operator =
                pSkipToken opTok |>> fun c leftTree rightTree -> DCall(DCall((BuiltInFunc operator), leftTree), rightTree)
            let pAdd = pOp (TokBuiltInOp (Arithm Add)) (Arithm Add)
            let pSubtract = pOp (TokBuiltInOp (Arithm Subtract)) (Arithm Subtract)
            let pMultiply = pOp (TokBuiltInOp (Arithm Multiply)) (Arithm Multiply)
            let pDivide = pOp (TokBuiltInOp (Arithm Divide)) (Arithm Divide)

            let pLessThan = pOp (TokBuiltInOp (Comp Lt)) (Comp Lt)
            let pLessThanOrEq = pOp (TokBuiltInOp (Comp Le)) (Comp Le)
            let pGreaterThan = pOp (TokBuiltInOp (Comp Gt)) (Comp Gt)
            let pGreaterThanOrEq = pOp (TokBuiltInOp (Comp Ge)) (Comp Ge)
            let pEqualTo = pOp (TokBuiltInOp (Comp Eq)) (Comp Eq)
            let pNotEqualTo = pOp (TokBuiltInOp (Comp Ne)) (Comp Ne)

            // Define precedence of basic BuiltInType operators
            let pAllOp = pMultiply <|> pDivide <|> pAdd <|> pSubtract
            let pSubTerm = pBracketed <|> pListFunctionApp <|> pVariable <|> pConst
            let pChainOperatorApp = pChainlMin1 pSubTerm pAllOp
            let pCompOps = pLessThan <|> pLessThanOrEq <|> pGreaterThan <|> pGreaterThanOrEq <|> pEqualTo <|> pNotEqualTo

            let pChainedFuncApps = pChainlMin1 pChainOperatorApp pCompOps
            let! res = pChainedFuncApps
            return res
        }

    let pIfThenElse =
        parser {
            do! pSkipToken (TokSpecOp IF)
            let! condition = pVariable <|> pConst <|> pBracketed
            do! pSkipTokenOrFail (TokSpecOp COLON)
            do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            let! ifTrue = pAst
            do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            do! pSkipTokenOrFail (TokSpecOp ELSE)
            do! pSkipTokenOrFail (TokSpecOp COLON)
            do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            let! ifFalse = pAst
            return DCall(DCall(DCall(BuiltInFunc IfThenElse, condition), ifTrue), ifFalse)
        }

    let pVariableDef = 
        parser {
            let! (Variable name) = pVariable
            do! pSkipToken (TokSpecOp EQUALS)
            do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            let! definition = pAst
            do! pManyMin1 (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
            let! expr = pAst
            return FuncDefExp(name, definition, expr)
        }

    let pCall = 
        parser {
            let! left = pVariable <|> pBracketed
            let! right = pManyMin1 (pConst <|> pVariable <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pBracketed)
            return combineCalls left right
        }
        
    (pFuncDefExp <|> pIfThenElse <|> pLambda <|> pCall <|> pVariableDef <|> pOperatorApp <|> pListAppend <|> pListInsert
     <|> pListFunctionApp <|> pBracketed <|> pFullPair <|> pHalfPair <|> pEmptyPair <|> pConst <|> pVariable)


let parseCode =
    parser {
        let! res = pAst
        do! pMany (pSkipToken (TokWhitespace LineFeed)) |> ignoreList
        return res
    }

let Parse (input:list<Token>) = 
    let numTokens = input.Length
    let parseResult = pRun parseCode input
    match parseResult with
        | Some(tree, index) ->
            if index = numTokens then 
                tree
            else
                failwithf "Failed Parse: Not all tokens parsed. Check bracket pairs. %A last parsed" input.[index-1]    
        | None -> failwithf "Failed Parse: Check bracket pairs and Function Definitions"