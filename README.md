# Compiler

## Lexical Analyzer

This part is mainly used for lexical analysis with ML-Lex, which reads the tiger input file and generates the lexical tokens.

Supporting files:

***tokens.sig*** Signature of the Tokens structure.\n
***tokens.sml*** The Tokens structure, containing the token type and constructors that your lexer should use to build instances of the token type. It is important to do it in this way, so that things will still work when the “real” parser is attached to this lexer, and the “real” Tokens structure is used.\n
***errormsg.sml*** The ErrorMsg structure, useful for producing error messages with file names and line numbers.\n
***driver.sml*** A test scaffold to run your lexer on an input file. \n
***tiger.lex*** The beginnings of a real tiger.lex file. \n
***sources.cm*** A “makefile” for the ML Compilation Manager.\n
***tiger.lex*** Analyze Identifiers, Comments, Integer literal, String literal, Reserved words, and Punctuations.\n In this file, the comments are handled appropriately and the nested comments are allowed;\n Strings are flexible, where escape sequences are dealt with correctly;\n Errors are covered and dealt with reasonably combined with the EOF.\n
***main.sml*** A wrapper for the whole program, which resets the parameters at the beginning of every usage.

Run the Code with the following commands:\n
***CM.make "sources.cm";***\n
***Go.go "test1.tig";***\n
