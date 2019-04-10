# Compiler

## Liveness Analysis

### Overview
This section implements the MakeGraph module that turns a list of Assem instructions into a flow graph; and also implements the Liveness module using one-variable-at-a-time method.

### Running the code
1. Build with `CM.make "sources.cm";`
2. Create a test, for example `foo.tig`
3. Run the code with `Main.compile "<foo.tig>";`

### Supporting Files
1. All files from previous stage, listed in `semant/README.md`, `ir/README.md`, and `is/README.md`
2. `graph.sml`: Graph module.
3. `makegraph.sml`: Control Graph Generator from Assem instructions.
4. `flowgraph.sml`: The flow graph data structure.
5. `liveness.sml`: Interference Graph Generator using one-variable-at-a-time method.
