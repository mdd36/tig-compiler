# Compiler

## Intermediate Representation

### Overview
This section extends the semantic analysis section to create an IR tree for the code. A driver to run the code over a tiger file provided in `main.sml`.

### Running the code
1. Build with `CM.make "sources.cm";`
2. Create a test, for example `foo.tig`
3. Run the code with `Go.go "<foo.tig>";`

### Supporting Files
1. All files from previous stage, listed in `semant/README.md`
2. `translate.sml`: Converts expressions to IR. Separate methods for almost all IR types.
4. `mips-frame.sml`: Implementation of the FRAME signature for MIPS. Handles the abstraction of a stackframe for MIPS.
5. `temp.sml`: Taken from Appel, provides temps and labels for the IR.
6. `semant.sml`: Updated to add expression translation.
