# Compiler

## Semantic Analysis

### Overview
This section provides semantic analysis over the AST constructed in the last step using four mutually recursive functions. A drive to run the code over a tiger file provided in `main.sml`.

### Running the code
1. Build with `CM.make "sources.cm";`
2. Create a test, for example `foo.tig`
3. Run the code with `Go.go "<foo.tig>";`

### Supporting Files
1. All files from previous stage, listed in `README.parser.md`
2. `semant.sml`: Main file for this stage. Defines the four mutually recursive functions of transExp, transTy, transDec, and transVar.
    1. transExp: Type checks the program. Assuming that all variables are in scope, confirm that all assignments, comparison, function calls, etc are valid. Implements a basic lattice structure by having a bottom type used for errors and the `break` expression, but otherwise follows the typing specifications laid out by Appel.
    2. transTy: Translates a type declaration into a Types.ty. `NAME` type still exists.
    3. transDec: Adds declarations to the current `venv` and `tenv`. No modifications from the Appel specification.
    4. transVar: Determine if the referenced name is in scope, and that it can subscripted or indexed as done in the code. Follows Appel.
3. `types.sml`: Types that are used for type checking, kept from Appel's spec.
4. `env.sml`: Signature and definition for the Env type. Holds the types of entries into the `venv` and `tenv` tables, and defines some default values of those environments. Kept from Appel.
5. `table.sml/table.sig`: Wrapper of an ORD_MAP used to hold `tenv` and `venv`. Kept from Appel.
6. `translate.sml`: Taken from Appel, placeholder for IR usage.
