# Tiger Compiler

## Overview
Compiler passing all Appel cases, as well as custom cases. See the `test` folder for additional tests to confirm our extra credit additions.

### Code fixes from past stages
#### Lexer
* Removed the `\f` token
#### Parser
* `let in end` parses
#### Semant
* Writing to a for loop variable reports an error
* Line numbers added to error messages
#### IR/IS
* Test 8 runs correctly in SPIM
* Function labels now numbered rather than function names
* Static link logic fixed, traces correct number of levels
#### Liveness/Regalloc
* Function return values not ignored. See Test 1 and Test 4, failed test for this stage, to confirm.

## Running the code
In the SML repl, build the project and then run `Main.compile <foo.tig>`.

## Implemented Optimizations
1. Passing more than 4 arguments to a function by pushing them onto the stack
2. Spilling variables onto the stack when there are too many live registers at once
3. Register coalescing using the Briggs heuristic
[//]: # (4. Reaching definitions)
[//]: # (5. Dead code elemination)