signature REG_ALLOC =
sig 
	structure Frame: FRAME
	type allocation = Frame.register Temp.Table.table
	val alloc : Assem.instr list * Frame.frame ->
							Assem.instr list * allocation
end

structure Regalloc :> REG_ALLOC =
struct
	structure Worklist = DynamicArrayFn(struct open Array
				    type elem = L.IGraph.node
				    type vector = L.IGraph.node vector
				    type array = L.IGraph.node array
                             end)
	
	structure L = Liveness
	structure NodeMap = RedBlackMapFn(type ord_key=L.IGraph.node val compare=L.IGraph.compare) 
	structure NodeSet = RedBlackSetFn(type ord_key=L.IGraph.node val compare=L.IGraph.compare)
	
	structure MoveEdge =
	struct
        type ord_key = L.IGraph.node * L.IGraph.node
        fun compare((ni, nj), (ni', nj')) =
            (case IGraph.compare(ni, ni') of
                EQUAL => IGraph.compare(nj, nj') 
                | LESS => LESS
				| GREATER => GREATER)
    end
	
	structure MoveSet = RedBlackSetFn(MoveEdge) (*worklistMoves, activeMoves, coalescedMoves, constrainedMoves, frozenMoves*) (*to do*)

	val K=9
	
	fun alloc (assemlist, frame) =
		let 
			val (L.IGRAPH{graph = ig,
					   tnode = tnode,
					   gtemp = gtemp,
					   moves = moves}, node2tmps) = L.interferenceGraph(#1 (Makegraph.instr2graph assemlist))

			val simplifyWorklist = Worklist.array(0, L.IGraph.errorNode ig)
			val worklistMoves = Worklist.array(0, L.IGraph.errorNode ig)
			val freezeWorklist = Worklist.array(0, L.IGraph.errorNode ig)
			val spillWorklist = Worklist.array(0, L.IGraph.errorNode ig)
			
			
			
			fun build () = 
				let
					val nodes = L.IGraph.nodes ig
					val degreeMap = foldl (fn (node, m) => NodeMap.insert(m, node, ref (length (L.IGraph.adj node)))) NodeMap.empty nodes
					
					fun search (node, m) = NodeMap.insert(m, node, foldl (fn (edge, s) => if L.IGraph.eq(node, #1 edge) orelse L.IGraph.eq(node, #2 edge) 
																	then MoveSet.add(s, edge) else s) MoveSet.empty moves)
					
					val moveLists = foldl search NodeMap.empty nodes
					
					fun addedge (node, s) = 
						let 
							val adjs = L.IGraph.adj ig
							fun addoneedge (adj, s') = 
								let 
									val e1 = (node, adj)
									val e2 = (adj, node)
								in 
									if MoveEdge.member(s', e1) then s' else MoveEdge.add(MoveEdge.add(s', e1), e2)
								end
						in 
							foldl addoneedge s adjs
						end
					
					val adjSet = foldl addedge MoveEdge.empty nodes
				in
					(degreeMap, moveLists, adjSet)
				end
				
			fun NodeMoves node = NodeSet.intersection(valOf(NodeMap.find(moveLists, node)), NodeSet.union(worklistMoves, activeMoves))
			
			fun MoveRelated node = NodeSet.isEmpty (NodeMoves node)	
			
			fun AddEdge (u, v) = 
			
			fun Adjacent n =
			
			
			
			fun EnableMoves nodes = 
				let
					fun enable (node, (am, wm)) =  
						foldl (fn (m, (a, w)) => if NodeSet.member(a, m) then (NodeSet.delete(a, m), NodeSet.add(w, m)) else (a, w)) (am, wm) (NodeMoves node) 
				in 
					foldl enable (activeMoves, worklistMoves) nodes
				end
			
			fun DecrementDegree m = (*to do*)
				let
					val d = !(valOf(NodeMap.find(degreeMap, m)))
				in
					valOf(NodeMap.find(degreeMap, m)) := d-1;
				end
					
			
			fun MakeWorklist initial =
				let 
					val items = NodeSet.listItems initial
					fun insert (node, (spi, fre, sim)) = 
							if !(valOf(NodeMap.find(degreeMap, node))) >= K
							then (NodeSet.add(spi, node), fre, sim)
							else (
								if MoveRelated node
								then (spi, NodeSet.add(fre, node), sim)
								else (spi, fre, NodeSet.add(sim, node))
								)
				in
					foldl insert (NodeSet.empty, NodeSet.empty, NodeSet.empty) items
				end
			
			
		in
		
		
		
		end
			


end
