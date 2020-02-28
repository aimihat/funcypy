# Sample Individual Statement for Individual Code Submission

This folder contains my individual code submission for the High Level Programming module. The core module is in the file titled `Parser.fs`. There is also a Common module (`Common.fs`) which is intended to contain common elements used by other modules in the rest of the project. It uses `Dotnet Core 3.1`, with dependency on `Expecto.Fscheck`.

A parser combinator approach with computation expressions was used to tackle the parser from a different perspective than Tick 3. Parser combinators proved difficult to work with at first but ultimately made the parser and thereby the language grammar much easier to extend and define. 
 
# How will your code be used by team (if all goes well) including what order are modules in your team project compiled?

The code in this folder will be used by the team to parse a stream of tokens passed from the tokeniser to form an abstract syntax tree (AST). No other modules have been used from other team members in the development of the parser and all functions were written independently. While our team discussed strategies and approaches for each of the parts of the project, each person wrote code specifically for their own module (or multiple modules where applicable in the specific folder name).

# What help have you obtained/given others debugging or doing code review?

Code reviews allowed us to remove non-essential functions and elements of the code, and to refactor the project into a more compatible form with the other modules. While some refactoring will still have to take place after the individual code deadline, all the modules are mostly compatible and should work well together (if all goes well).

# How did you work out (who decided what - how do you revise) the types that interface your code with others?

As the parser is essentially what defines the language grammar, any revisions made to the AST or the type interfaces primarily happened while I was making the parser. This involved communication between all team members as sometimes tweaks had to be made to ease the load for the parser and the runtimes.

# Brief Code Overview

The `pExpr` is a top level parser, which can currently be used to parse function definition expressions, anonymous functions (lambdas), bracketed expressions, function application, conditionals, and operator precedence. The group phase may implement more language features using the same approach. No mutable values of any kind were used in the development as instructed.

You can run `pExpr` or any of the subfunctions using the helper function `pRun` (which runs a parser) in `Program.fs`.

F# Computation expression were used because they are a standard FP pattern that can be used to combine building block parsers to build much more complex parsers and eventually the language grammar.

The testing module (`Testing.fs`) contains a series of unit tests for the parser to ensure that it is returning the correct output type. These tests were used from early on in the development process to ensure test driven development. These units can currently be run from `Program.fs`.