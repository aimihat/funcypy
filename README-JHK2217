# FuncyPy: Lexer Module made by James Kim

Since the individual working phase, the Lexer Module became the first stepping ground of the whole program as was its intention. As per the team's demand during the individual working phase, the code was designed for each data type to have its own definition on the several maps declared in the beginning of the code - making it easy to add or subtract a new token of a given type at will. This design choice has been proven useful as it has solved the largest problem of the Lexer at the time (the problem of parsing a list) by the simple addition of LSB, RSB and COMMA tokens.

After the first few initial group meetings where we have shared, described and edited each other's code, fixes to the Lexer were mostly made of bug fixes and personal coding preferences - mostly requested by Yannis and the parser, as is the only module immediately following the lexer, but also rarely receiving requests by the rest of the team as well. A list of the major changes made to the Lexer are as follows:

- Add/delete entries for the token maps
- Made an ignore list: characters can now be ignored entirely (used for whitespaces)
- Re-define dashID to accomodate the parser's exact needs
- Re-define extractWord to allow alphanumeric restriction to identifier names
- Re-define extractWord to fail lexing certain characters that do not belong in the code (e.g. %,@,£,¢,§,ˆ,¶ etc.)
- Comment capabilities: Lexer will ignore all characters between ("//" and "\n") and ("/*" and "*/")
- Fail the lexing for an empty program (excluding ignored characters)
- etc..
