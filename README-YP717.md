# Yannis Panagis (YP717) Individual README for Group Phase

I worked primarily on fixing bugs and improving the functionality of the code I wrote for the individual phase. I also worked on integrating the parser with the lexer and combinator runtime. This meant writing lexer,parser, and end-to-end unit tests to find edge cases and improve the code's ability to handle errors. 

Git was used as a means of version control throughout the group phase. Furthermore, the group decided to limit access to pushing to the master branch because another group member over-wrote my individual folder when pushing their code during the individual submission phase. A PR-based approach was followed to move code from working branches to the main master branch. 

In summary, some of the features and fixes I worked on during the group phase include but are not limited to
- Fixed Function definition expression format in parser
- Improved if-then-else: simplified language syntax so no need for endif
- Added variable definition without def keyword e.g. x=5
- Improved Parser error handling to indicate token of failure
- Fixed operator precedence
- Added list(and pair) functionality to parser and lexer
- Added list function functionality to parser and lexer
- Test bench design for parser and lexer
- Attempted property-based tests for parser
- Wrote Lexer unit tests with Expecto
- Wrote Parser unit tests with Expecto
- Wrote End-to-end unit tests with Expecto
- General code cleanup, bug fixes, and improvements
- Parser improvements with help from Aimilios
- Took a leadership role to try to keep the group focused and moving with the project
