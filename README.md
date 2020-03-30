# **Team Work Plan ⁠— FuncyPy (FP Language)**

#### Yannis Panagis - Sergey Chirkunov - Aimilios Hatzistamou - James Kim

Our team’s deliverable is a functional language called **FuncyPy**. It uses a simple syntax to make functional programming more accessible to developers coming from object oriented languages with minimal syntax like Python. In our design, we aim to utilize Python syntax in functional paradigm (partial-application, no loops, immutable types) to make FP more accessible to people coming from an OOP background. For sample code syntax please refer to the appendix. 

## Description

The project codebase is broken down into different modules following the structure below and tracked with Github for version control. All major functions are tested using automated test cases and property-based tests where possible.

- **Helper Module (Common)** | Includes common type definitions and common helper functions that are used across modules, such as functions that convert between types, and printing functions for debugging.
- **Lexer Module (James)** **|** **Lexer: string -> List Tokens** | Tokenizes inputs into specific types to be evaluated by the parser into an Abstract Syntax Tree. Tokens include types such as identifiers, keywords, sperators, operators, literals and comments.
- **Parser Module (Yannis) |** **Parser: Token list** **-> Result** | Uses parser combinators and computation expressions to design a flexible language grammar. Parser was initially based on the more generic type parser (similar to Tick 3 extension) with parser combinators and now parses a stream of tokens from the lexer into an abstract syntax tree.
- **Combinator Runtime System (Aimilios) |** **CombinatorRS: AstT -> ‘T** | Evaluates expressions with arguments first, except for if-else conditionals which will evaluate condition-first. Input to the run-time is AST output from parser. All functions will be Curried as lambdas, with memoization used throughout reduction (no function applied twice to same arguments).

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

**Team deliverable assessment: individual contributions**

1. **Aimilios**: Runtime improvements, system integration, parser-runtime Integration, CLI, standard lib, basic recursion, demo code
2. **Yannis**: Parser bug fixes and improvements, parser-lexer Integration, parser-runtime Integration, lexer unit tests, parser tests
3. **Sergey**: Arithmetic Operators Property-based Tests, Combinator Z Recursion (attempted), Lambda run-time (individual)
4. **James**: Lexer bug fixes and improvements, lexer unit tests, Lexer 

Please see Individual README statements root of Master branch for more details on individual contributions during group phase.

After the group presentation, we also made changes to allow mutual recursion and included a demo of this as well.
