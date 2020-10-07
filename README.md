# **FuncyPy (Functional programming language)**

**FuncyPy** uses a simple syntax to make functional programming more accessible to developers coming from object-oriented languages with minimal syntax like Python. In our design, we aim to utilize Python's syntax in functional paradigm (partial-application, no loops, immutable types).

## Code structure

- **Helper Module** | Includes common type definitions and common helper functions that are used across modules, such as functions that convert between types, and printing functions for debugging.
- **Lexer Module** **|** **Lexer: string -> List Tokens** | Tokenizes inputs into specific types to be evaluated by the parser into an Abstract Syntax Tree. Tokens include types such as identifiers, keywords, sperators, operators, literals and comments.
- **Parser Module |** **Parser: Token list** **-> Result** | Uses parser combinators and computation expressions to design a flexible language grammar. Parser was initially based on the more generic type parser (similar to Tick 3 extension) with parser combinators and now parses a stream of tokens from the lexer into an abstract syntax tree.
- **Combinator Runtime System |** **CombinatorRS: AstT -> ‘T** | Evaluates expressions with arguments first, except for if-else conditionals which will evaluate condition-first. Input to the run-time is AST output from parser. All functions will be Curried as lambdas, with memoization used throughout reduction.

All major functions are tested using automated test cases and property-based tests where possible.

## Appendix

### **Syntax Demo for FuncyPy**

The following is an brief sample of the syntax of FuncyPy.

```F#
def factorial x:
    if (x==0):
        1 
    else:
        x * (factorial (x-1))

y = 3
y + (factorial 4)

>> 27
```

## Build instructions

**Windows**

Pull the Repository:
```git clone https://github.com/AimiHat/FuncyPy```

- open CLI
```Windows + R -> cmd -> Enter```

- Run build.bat
```build```

- For running a test code you need to type funcypy followed by a file name:
```funcypy simple_test.fpy```

**Mac OSX**

To run tests in development mode, set RUN_TESTS in Program.fs to true and comment out the release code below. To run release code set RUN_TESTS to false.
