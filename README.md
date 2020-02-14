# **Team Work Plan ⁠— FuncyPy (FP Language)**

#### Yannis Panagis - Sergey Chirkunov - Aimilios Hatzistamou - James Kim

Our team’s deliverable is a functional language called **FuncyPy**. It uses a simple syntax to make functional programming more accessible to developers coming from object oriented languages with minimal syntax like Python. In our design, we aim to utilize Python syntax in functional paradigm (partial-application, no loops, immutable types) to make FP more accessible to people coming from an OOP background. For sample code syntax please refer to the appendix. 

## Description

The project codebase will be broken down into different modules following the structure below and tracked with Github for version control. To begin with, each team member was assigned a unique module to work on; adjustments could be made later to balance load, if necessary. All major functions will be tested using automated test cases and property-based tests where possible.

- **Helper Module (Common)** | The helper module will include common type definitions and common helper functions that are used across modules, such as functions that convert between types, and printing functions for debugging.
- **Lexer Module (James)** **|** **Lexer: string -> List Tokens** | The lexer module will tokenize inputs into specific types to be evaluated by the parser into an Abstract Syntax Tree. The Tokens will include token types such as identifiers, keywords, sperators, operators, literals and comments. An extension goal for this module will be to abstain from using any .NET features or types such as the .NET floating-point type which is used by F# so that everything is FABLE compatible and it can port to other languages like JavaScript.
- **Parser Module (Yannis) |** **Parser: Token list** **-> Result** | The parser will initially be based on the more generic type parser (similar to Tick 3 extension) with parser combinators and will parse a stream of tokens from the lexer into an abstract syntax tree (see Pratt Parser extension).
- **Combinator Runtime System (Aimilios) |** **CombinatorRS: AstT -> ‘T** | The runtimes will evaluate expressions with arguments first, except for if-else conditionals which will evaluate condition-first. Input to the run-time will be the environment, with optionally the extended standard library as in extension below below. All functions will be Curried as lambdas, with memoization used throughout reduction (no function applied twice to same arguments).

- **Lambda Evaluation Runtime System (Sergey) |** **Module LambdaEvalRS: AstT -> ‘T** | Lambda body will be evaluated recursively. This requires: Dynamic name values, that can be resolved by holding the environment as a list of name-value pair. (i.e type list = item of name\*value | item\*list), and implementing closures such that all arguments are looked up and used. Finally the language should know when to evaluate. 

- **Main Module:** F# Modules cannot contain references to later defined values so including a ‘main module’ at the end of the pipeline will provide a frame to import all other modules and bring everything together.

## **Potential Extensions (order of priority)**

We are proposing several possible extensions for our functional language. Which ones to pursue will be re-evaluated once further progress has been made with the code. All extensions will be completed in pairs because adding a few working extensions is a priority over adding many just for the sake of it.

- **Pratt parser**: Rewrite the parser to associate semantics with tokens instead of grammar rules to improve the base recursive descent parser. This would allow support for operator precedence, right associativity, using parantheses to override operator precedence and defining operators to be both infix and unary.
- **Neater syntax:** Further syntax simplification by removing superfluous constructs such as ‘endif’ and adding constructs like ‘elif’.
- **GUI Module:** A Visual2-like interface would allow for a better demonstration of the language in an easy-to-compile and run environment. An action point here is to ask for access to the VisUAL 2 code so that we can use it as a basis for this extension.
- **Standard Library:** Built-in functions and modules, implemented in FuncyPy would improve practical utility of the language. This would include functions like F#’s List.map and List.reduce, which could be implemented using lists in the core language syntax.
- **Python integration:** Enabling support for importing certain python modules directly in our code. These would be evaluated in run-time as python subprocesses (requires matching types, currying would fail). E.g. working interface with matplotlib’s pyplot interface (to plot FuncyPy lists).

## Appendix

### Preliminary Type Definitions (AST)

```F#
// Incomplete type definition, closures implemention TBD

type Arithmetic = Add | Subtract | Multiply | Divide
type Comparison = Eq | Ne | Lt | Gt | Le | Ge
type Identifier = string
 
type Value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Tuple of Value*Value
    | List of Value*Tuple
 
type Ast =    
    | Statement of Ast    
    | Expression of Ex    
    | Function of string option * Argument list option * Ast
    | Scope of Ast list option
    | Conditional of Ex * Ast * Ast option
    | Call of identifier * Argument list option
    | Assign of identifier * Ex
and Ex =
    | Single of Ast
    | Literal of Value
    | Variable of Identifier
    | Arithmetic of Ex * Arithmetic * Ex
    | Comparison of Ex * Comparison * Ex
and Argument =
    | Element of Ex
```

### **Syntax Demo for FuncyPy**

The following is an (incomplete) sample of the syntax of FuncyPy.

```F#
y = 2 // int type	
a = "test" // string type
b = True  // boolean type
d = (True, "test") // Tuple type (e.g. bool, string)
e = [True, False, False] // list of bool
f = 2+2 // example arithmetic expression

lambda x: 2 * x // anonymous function definition

def f x y: // Curried function definition
	if y==2:
		True
	elif y==1: // (extension)
		False
	else:
		False
	endif // (remove as extension)

```

