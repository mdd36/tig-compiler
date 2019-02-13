# Compiler

## Paser and AST

This part is mainly used for syntax analysis and then generating the AST.

Supporting files:


***errormsg.sml*** The ErrorMsg structure, useful for producing error messages with file names and line numbers.<br />
***sources.cm*** A “makefile” for the ML Compilation Manager.<br />
***tiger.lex*** Analyze Identifiers, Comments, Integer literal, String literal, Reserved words, and Punctuations.<br /> In this file, the comments are handled appropriately and the nested comments are allowed;<br />Strings are flexible, where escape sequences are dealt with correctly;<br />Errors are covered and dealt with reasonably combined with the EOF.<br />
***main.sml*** A wrapper for the whole program, which resets the parameters at the beginning of every usage.<br />
***tiger.grm*** Illustrate the grammar and the rules with no reduce/reduce conflicts or reduce/shift conflicts.<br />
***absyn.sml*** The abstract syntax data structure for Tiger.<br />
***printabsyn.sml*** A pretty-printer for abstract syntax trees, so you can see your results.<br />
***symbol.sml*** A module to turn strings into symbols . This file uses components of the Standard ML of New Jersey Library . Look them up in the library manual to see what’s going on.<br />
***parse.sml*** A driver to run your parser on an input file.<br />

Run the Code with the following commands:<br />
***CM.make "sources.cm";*** <br />
***Go.go "test1.tig";*** <br />

Example for test1.tig:<br /><br />
![alt text](https://github.com/HanxinHua/Compiler/blob/lexing/test.png)
