# Compiler

## Instruction Selection

### Overview
This section implements the translation to Assem-instructions for MIPS instruction set

### Running the code
1. Build with `CM.make "sources.cm";`
2. Create a test, for example `foo.tig`
3. Run the code with `Main.compile "<foo.tig>";`

### Supporting Files
1. All files from previous stage, listed in `semant/README.md` and `ir/README.md`
2. `canon.sml`: Canonicalization and trace-generation.
3. `assem.sml`: The Assem module.
4. `main.sml`: A main module that you may wish to adapt.
