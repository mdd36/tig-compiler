# Compiler

## Register Allocation

### Overview
This section implements graph coloring register allocation as two modules: Color , which does just the graph coloring itself, and RegAlloc , which manages spilling and calls upon Color as a subroutine.

### Running the code
1. Build with `CM.make "sources.cm";`
2. Create a test, for example `foo.tig`
3. Run the code with `Main.compile "<foo.tig>";`

### Supporting Files
1. All files from previous stage, listed in `semant/README.md`, `ir/README.md`, `is/README.md`, and `dataflow/README.md`
2. `regalloc.sml`: Allocate the registers for temporaries.
3. `color.sml`: Color the graph with the implementation of spilling and coalescing.
