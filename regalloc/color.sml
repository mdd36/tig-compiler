signature COLOR =
sig 
	structure Frame: FRAME
	
	type allocation = string Temp.Table.table
	
	val color : {interference: Liveness.igraph,
				 initial: allocation,
				 spillCost: Graph.node -> int,
				 registers: Frame.register list}
				  -> allocation * Temp.temp list
end

structure Color :> COLOR =
struct

	type allocation = string Temp.Table.table
	open Util
	exception NotEnoughColors of string

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
	
    structure RegSetKey = 
    struct
    	type ord_key = Frame.register
    	fun compare = String.compare
    end

	structure MoveSet = RedBlackSetFn(MoveEdge) (*worklistMoves, activeMoves, coalescedMoves, constrainedMoves, frozenMoves*) (*to do*)
	structure RegSet = ListSetFn()
	structure Stack =
	struct 
		type stack =  L.IGraph.node * int list
		
		val empty = ref [] : stack ref
		fun push (a, s) = s:=a::(!s)
		fun pop (ref []) = NONE
		  | pop s  = let val a::m = (!s)
						in
						(s:= m; SOME a)
						end
		fun items s = NodeSet.addList(NodeSet.empty, !s) 
	end
	
	val K = 9 (* 9 temp reg's in MIPS *)

	
	fun color {interference as IGRAPH.Liveness{graph, tnode, gtemp, moves}, initial, spillCost, registers} =

		let

			val (L.IGRAPH{graph = ig,
			   tnode = tnode,
			   gtemp = gtemp,	
			   moves = moves}, node2tmps) = L.interferenceGraph(#1 (Makegraph.instr2graph assemlist))

			val simplifyWorklist = ref (NodeSet.empty) (*1*)
			val freezeWorklist = ref (NodeSet.empty) (*2*)
			val spillWorklist = ref (NodeSet.empty) (*3*)
			
			val worklistMoves = ref (MoveSet.addList(MoveSet.empty, moves))
			val activeMoves = ref (MoveSet.empty)
			val frozenMoves = ref (MoveSet.empty)
			val constrainedMoves = ref (MoveSet.empty)
			val coalescedMoves = ref (MoveSet.empty)

	
			fun build () = 
				let
					val nodes = L.IGraph.nodes ig
					val degreeMap = foldl (fn (node, m) => NodeMap.insert(m, node, ref (length (L.IGraph.adj node)))) NodeMap.empty nodes
					
					fun search (node, m) = NodeMap.insert(m, node, foldl (fn (edge, s) => if L.IGraph.eq(node, #1 edge) orelse L.IGraph.eq(node, #2 edge) 
																	then MoveSet.add(s, edge) else s) MoveSet.empty moves)
					
					val moveLists = foldl search NodeMap.empty nodes
					val adjList = foldl (fn (a, m) => NodeMap.insert(m, a, ref (NodeSet.addList(NodeSet.empty, L.IGraph.adj a)))) NodeMap.empty nodes
					val (precolored, initial) = foldl (fn (n, (pre, ini)) => (let val tmp = gtemp n in
																				if isSome(Temp.look(MipsFrame.tempMap, tmp))
																				then (NodeSet.add(pre, n), ini)
																				else (pre, NodeSet.add(ini, n)))) (NodeSet.empty, NodeSet.empty) nodes
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
					(degreeMap, moveLists, adjSet, adjList, precolored, initial)
				end
				
			val (degreeMap, moveLists, adjSet, adjList, precolored, initial) = build()
			val adjSet = ref adjSet
			val spilledNodes = ref (NodeSet.empty)
			val coalescedNodes = ref (NodeSet.empty)
			val coloredNodes = ref (NodeSet.empty)
			val colorMap = ref (Temp.Table.empty)
			val selectStack = Stack.empty
			
			val alias = ref (NodeMap.empty)
			val realias = ref (NodeMap.empty)
			
			fun NodeMoves node = case NodeMap.find(realias, node) of NONE => MoveEdge.intersection(valOf(NodeMap.find(moveLists, node)), MoveEdge.union(!worklistMoves, !activeMoves))
																| SOME m =>MoveEdge.union(NodeMoves m, MoveEdge.intersection(valOf(NodeMap.find(moveLists, node)), MoveEdge.union(!worklistMoves, !activeMoves)))
				
			fun MoveRelated node = MoveEdge.isEmpty (NodeMoves node)	
			
			fun AddEdge (u, v) = 
				if MoveEdge.member(!adjSet, (u, v)) andalso L.IGraph.eq(u,v)
				then ()
				else (adjSet := MoveEdge.add(MoveEdge.add(!adjSet, (v, u)), (u, v));
						case (NodeSet.member(precolored, u), NodeSet.member(precolored, v)) 
						of (true, true) => ()
						| (false, true) => (valOf(NodeMap.find(addList, u)) := NodeSet.add(!(valOf(NodeMap.find(addList, u))), v);
											valOf(NodeMap.find(degreeMap, u)) := !(valOf(NodeMap.find(degreeMap, u)))+1)
						| (true, false) => (valOf(NodeMap.find(addList, v)) := NodeSet.add(!(valOf(NodeMap.find(addList, v))), u);
											valOf(NodeMap.find(degreeMap, v)) := !(valOf(NodeMap.find(degreeMap, v)))+1)
						| (false, false) => (valOf(NodeMap.find(addList, u)) := NodeSet.add(!(valOf(NodeMap.find(addList, u))), v);
											valOf(NodeMap.find(degreeMap, u)) := !(valOf(NodeMap.find(degreeMap, u)))+1;
											valOf(NodeMap.find(addList, v)) := NodeSet.add(!(valOf(NodeMap.find(addList, v))), u);
											valOf(NodeMap.find(degreeMap, v)) := !(valOf(NodeMap.find(degreeMap, v)))+1)
						)
						
						
			
			fun Adjacent n = NodeSet.listItems (NodeSet.difference(!(valOf(NodeMap.find(adjList, n))), NodeSet.union(Stack.items selectStack, !coalescedNodes)))			
			
			
			fun EnableMoves nodes = 
				let
					fun enable (node, ()) =  
						foldl (fn (m, ()) => if MoveEdge.member(!activeMoves, m) then (activeMoves := MoveEdge.delete(!activeMoves, m),
															worklistMoves := MoveEdge.add(!worklistMoves, m)) else ()) () (NodeMoves node) 
				in 
					foldl enable () nodes
				end
			
			fun DecrementDegree m = 
				let
					val d = !(valOf(NodeMap.find(degreeMap, m)))
				in
					valOf(NodeMap.find(degreeMap, m)) := d-1;
					if d = K
					then (EnableMoves(m::(Adjacent m));
						  spillWorklist:=NodeSet.delete(!spillWorklist, m);
						  if MoveRelated m
						  then freezeWorklist:=NodeSet.add(!freezeWorklist, m)
						  else simplifyWorklist:=NodeSet.add(!simplifyWorklist,m))
					else ()
				end
					
			
			fun MakeWorklist initial =
				let 
					val items = NodeSet.listItems initial
					fun insert (node, ()) = 
							if !(valOf(NodeMap.find(degreeMap, node))) >= K
							then spillWorklist := NodeSet.add(!spillWorklist, node)
							else (
								if MoveRelated node
								then freezeWorklist := NodeSet.add(!freezeWorklist, node)
								else simplifyWorklist := NodeSet.add(!simplifyWorklist, node))
								)
				in
					foldl insert () items
				end
			
			fun Simplify () =
				let 
					val n = NodeSet.listItems(!simplifyWorklist)
					fun sim (node,()) =
						(simplifyWorklist:=NodeSet.delete(!simplifyWorklist, node);
						 Stack.push((node,1), selectStack);
						 map DecrementDegree (Adjacent node))
				in
					sim (hd n)
				end
				
			(* coalesce **********************************************)
			
			fun GetAlias n =
				if NodeSet.member(!coalescedNodes) 
				then GetAlias(valOf(NodeMap.find(!alias, n)))
				else n
				
			fun OK (t, r) = valOf(NodeMap.find(degreeMap, t)) < K orelse 
									NodeSet.member(precolored, t) orelse
									MoveEdge.member(!adjSet, (t,r))
									
			fun Conservative nodes =
				let val k = ref 0
					fun judge (node, ()) = if valOf(NodeMap.find(degreeMap, node)) >= K then k:=(!k)+1 else ()
				in (foldl judge () nodes;
					k<K)
				end
			
			fun AddWorkList u =
				if (not (NodeSet.member(precolored, u))) andalso(not (MoveRelated u)) andalso valOf(NodeMap.find(degreeMap,u))<K
				then (freezeWorklist:=NodeSet.delete(!freezeWorklist, u);
					  simplifyWorklist:=NodeSet.add(!simplifyWorklist, u))
				else ()
			
			fun Combine (u,v) =
				let 
					fun uni t = 
						(AddEdge (t,u); DecrementDegree t)
				in
					(if NodeSet.member(!freezeWorklist, v)
					 then freezeWorklist:=NodeSet.delete(!freezeWorklist,v)
					 else spillWorklist:=NodeSet.delete(!spillWorklist, v);
					 coalescedNodes:=NodeSet.add(!coalescedNodes, v);
					 alias:= NodeMap.insert(!alias, v, u);
					 realias:= NodeMap.insert(!realias, u, v);
					 map uni (Adjacent v);
					 if valOf(NodeMap.find(degreeMap, u)) >= K andalso NodeSet.member(!freezeWorklist, u)
					 then (freezeWorklist:=NodeSet.delete(!freezeWorklist, u);
						   spillWorklist:=NodeSet.add(!spillWorklist, u))
					 else ()
					 )
				 end
			
			fun Coalesce () =
				let
					val moveedges = MoveEdge.listItems (!worklistMoves)
					fun divide (move as (m, n)) =
						let
							val x = GetAlias m
							val y = GetAlias n
							val (u, v) = if NodeSet.member(precolored, y) then (y, x) else (x, y)
						in
							(worklistMoves := MoveEdge.delete(!worklistMoves, move);
							if L.IGraph.eq(u,v)
							then (coalescedMoves:=MoveEdge.add(!coalescedMoves, move);
								  AddWorkList u)
							else (
								  if NodeSet.member(precolored, v) orelse MoveEdge.member(!adjSet, (u,v))
								  then (constrainedMoves:=MoveEdge.add(!constrainedMoves, move);
										AddWorkList u;
										AddWorkList v)
								  else (if (NodeSet.member(precolored, u) andalso (foldl (fn (a, b) => b andalso OK(a, u)) true (Adjacent v)))
											orelse ((not (NodeSet.member(precolored, u))) andalso Conservative((Adjacent u)@(Adjacent v)))
										then (coalescedMoves:=MoveEdge.add(!coalescedMoves, move);
											  Combine(u, v);
											  AddWorkList u)
										else (activeMoves:=MoveEdge.add(!activeMoves, move)))))
						end
				in
					divide (hd moveedges)
				end
							
			(* freeze **********************************************)
			
			fun FreezeMoves u =
				let 
					val mnodes = MoveEdge.listItems (NodeMoves u)
					fun fre (m as (x,y)) = 
						let 
							val v = if (GetAlias y) = (GetAlias u) then (GetAlias x) else (GetAlias y)
						in
							activeMoves:=MoveEdge.delete(!activeMoves, m);
							frozenMoves:=MoveEdge.add(!frozenMoves, m);
							if MoveRelated(v) andalso valOf(NodeMap.find(degreeMap, v)) < K
							then (freezeWorklist:=NodeSet.delete(!freezeWorklist, v);
								  simplifyWorklist:=NodeSet.add(!simplifyWorklist, v))
							else ()
						end
				in
					map fre mnodes
				end
			
			fun Freeze () =
				let 
					val u = hd (NodeSet.listItems (!freezeWorklist))
				in
					freezeWorklist:=NodeSet.delete(!freezeWorklist, u);
					simplifyWorklist:=NodeSet.add(!simplifyWorklist, u);
					FreezeMoves u
				end
					
			
			(* spill **********************************************)
			
			
			fun SelectSpill () =
				let
					val l = NodeSet.listItems (!spillWorklist)
					val m = #1 (foldr (fn (n, c) => let val sc = spillcost n in if sc < #2 c then (n, sc) else c end) ((hd  l), spillcost (hd l)) (tl l))
				in 
					spillWorklist:=NodeSet.delete(!spillWorklist, m);
					simplifyWorklist:=NodeSet.add(!simplifyWorklist, m);
					FreezeMoves m
				end
			
			
			
			(****************)
			fun repeat () = 
				if not (NodeSet.empty(!simplifyWorklist)) then (Simplify();repeat())
				else if not (MoveEdge.empty(!worklistMoves)) then (Coalesce();repeat())
				else if not (NodeSet.empty(!freezeWorklist)) then (Freeze();repeat())
				else if not (NodeSet.empty(!spillWorklist)) then (SelectSpill();repeat())
				else ()

			(**** COLORING ****)


			fun tryColoring() = (case (!selectStack) of
					nil => 
						let
							fun f node = 
								let
									val tmp = gtemp node
									val aliasNode = GetAlias node
									val aliasTmp = gtemp aliasNode
									val c = valOf(Temp.Table.look(!colorMap, aliasTmp))
								in
									colorMap := Temp.Table.enter(!colorMap, tmp, c)
								end		
						in (
							NodeSet.app f (!coalescedNodes);
							!colorMap
							)
						end
				|	node::tail => 
						let
							val adj' = L.IGraph.adj node
							val allRegisters = RegSet.addList(RegSet.empty, registers)
							fun f (node, availColors) = 
								let
									val tmp = gtemp node
									val node' = GetAlias node
									val tmp' = gtemp node'
									val allColoredNodes = NodeSet.union(!coloredNodes, !precolored)
								in
									if NodeSet.member(allColoredNodes, tmp') then
										let
											val c = Temp.Table.look(!colorMap, tmp')
										in
											if isSome c andalso RegSet.member(availColors, valOf c) then
												RegSet.delete(availColors, c)
											else raise ErrorMsg.Error "Unknown color provided"
										end
									else availColors
								end

							val availColors = foldr f allRegisters adj'
						in
							selectStack := tail
							if RegSet.isEmpty availColors then
								spilledNodes := NodeSet.add(!spilledNodes, node)
							else (
								coloredNodes := coloredNodes.add(!coloredNodes, node);
								colorMap := Temp.Table.enter(!colorMap, tmp, (hd (RegSet.listItems availColors)))
								)
							tryColoring()
						end
				)
		in
			build();
			MakeWorklist();
			repeat();
			(tryColoring(), map gtemp (NodeSet.listItems (!spillNS))
		end
end
