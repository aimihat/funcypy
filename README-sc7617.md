# README - sc7617
My folder organised as the following:
-- \Lambda - Lambda project and **Lambda.fs** file. This project is defined as a module to future main project
-- \Lambda.Tests - This folder contains **Lambda.Tests.fsproj** for **Lambda.fsproj** and **Lambda.Tests.fs** file. The project must be run from as **Lambda.Tests.fsproj**
-- \Junk - This folder contains unused files (mainly scripts) previously used to test language properties and various ideas
-- **MyLambda.fsx** is a script where early version of **Lambda.fs** used to be. Used for rapid development.

## How will your code be used by team (if all goes well) including what order are modules in your team project compiled?
My code is capable of evaluating lambda expression for our programming language (FuncyPy). The module I wrote (Lambda.fs) evaluates lambda to a literal value (i.e. 1, 10.1, "Hello World", True ) or returns an error. My error message is constructed such that it either returns a part of the code that failed or purely evaluated lambda saying that there is no literal evaluation, but there is a pure evaluation available. My module can arithmetic calculation and conditional statements.

## Which parts if any are code written for other people?
There are no particular parts written for other people. However, I made functions for arithmetic calculations that can be used in the main.

## Which parts if any of code you use is written by others?
There are no such parts.

## What help have you obtained/given others debugging or doing code review?

At the beginning and during early development we discussed our code such so it will be easier to make it all work together at the end. I worked closely with Yannis (Parsing) and we agreed on grammar, error handling and lambda representation. A couple of times we did a code review and discussed better F# programming practices for our solutions. Closer to the deadline I discussed generator implementation with Aimilious and Yannis in the group chat, received help from Aimilious on how to test property generating random AST and shared my solution on making a custom generator of a particular AST (i.e. Arithmetics generator).

## How did you work out (who decided what - how do you revise) the types that interface your code with others?

We opened regular discussion to make sure that our code will work well together. I talked to Yannis, almost on regular bases about grammar he defines. I asked him for particular lambda definition and he told me that his parser returns **Result<Ast*int,string> list** where **int** represent token position. This means that later I will need to adjust my post parsing so it reads input correctly.
