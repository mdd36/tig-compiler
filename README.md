# Tiger Compiler

## Overview
Compiler passing all Appel cases, as well as custom cases. See the `test` folder for additional tests to confirm our extra credit additions.

## Running the code
In the SML repl, build the project and then run `Main.compile <foo.tig>`.

## Implemented Optimizations
1. Passing more than 4 arguments to a function by pushing them onto the stack
2. Spilling variables onto the stack when there are too many live registers at once
3. Register coalescing using the Briggs heuristic  
4. Reaching definitions including constant propagation and copy propagation
5. Dead code elemination
