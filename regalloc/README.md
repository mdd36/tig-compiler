# Compiler

## Register Allocation

### Overview
This section implements graph coloring register allocation as two modules: Color , which does just the graph coloring itself, and RegAlloc , which manages spilling and calls upon Color as a subroutine. This allocator supports spilling to the stack, seen in spill-test.tig, and node coalescing under very consevative circumstances, seen in coalesce.tig. When choosing what to spill, the node with the fewest defs/uses and most adjacents is chosen, and any temp that holds a loaded value from a spill has infinite cost to spill again. Our allocator also makes a final pass over the allocated assembly and removes most "stupidInstrcutrions," like
```
	sw $t0, -4($fp)
	lw $t0, -4($fp)
	...
	move $t0, $t0
```

We determine if a memory instruction is stupid by comparing everything except the op command, or in the above example, we'd compare
```
	$t0, -4($fp)
	$t0, -4($fp)
```
Since we only look one head and sw doesn't write to any registers, the value of $fp cannot change between the load and store, so we can ensure that the memory address is the same between the two. The instruction if formatted with the final allocation duirng this process to ensure that the analysis is correct. Only the load is deleted, since we might try to load the value later.

The move is trivial to compare and remove, as we just check the src and dst for equality.

Spill cost to spill a temp is
```
	if isSpillTemp then inf else (numDefs + numUses) / numAdjInIgraph
```

### Running the code
1. Build with `CM.make "sources.cm";`
2. Create a test, for example `foo.tig`
3. Run the code with `Main.compile "<foo.tig>";`

### Supporting Files
1. All files from previous stage, listed in `semant/README.md`, `ir/README.md`, `is/README.md`, and `dataflow/README.md`
2. `regalloc.sml`: Allocate the registers for temporaries.
3. `color.sml`: Color the graph with the implementation of spilling and coalescing.
