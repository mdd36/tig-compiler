signature COLOR =
sig 
	structure Frame: FRAME
	
	type allocation = string Temp.Table.table
	
	val color : {interference: Liveness.igraph,
				 initial: allocation, (* FIXME never use this *)
				 spillCost: Liveness.IGraph.node -> int,
				 registers: Frame.register list}
				  -> allocation * Temp.temp list
end

structure Color :> COLOR =
struct

	type allocation = string Temp.Table.table
	open Util
	exception NotEnoughColors of string

	structure L = Liveness
	structure Frame = MipsFrame
	structure NodeMap = RedBlackMapFn(type ord_key=L.IGraph.node val compare=L.IGraph.compare) 
	structure NodeSet = RedBlackSetFn(type ord_key=L.IGraph.node val compare=L.IGraph.compare)
	
	structure MoveEdge =
	struct
        type ord_key = L.IGraph.node * L.IGraph.node
        fun compare((ni, nj), (ni', nj')) =
            (case L.IGraph.compare(ni, ni') of
                EQUAL => L.IGraph.compare(nj, nj') 
                | LESS => LESS
				| GREATER => GREATER)
    end
	
    structure RegSetKey = 
    struct
    	type ord_key = Frame.register
    	val compare = String.compare
    end

	structure MoveSet = RedBlackSetFn(MoveEdge) 
	structure RegSet = ListSetFn(RegSetKey)
	structure Stack =
	struct 
		type stack =  L.IGraph.node list 
		
		val empty: stack ref = ref [] 
		fun push (a, s) = s:=a::(!s)
		fun pop (ref []) = NONE
		  | pop s  = let val a::m = (!s)
						in
						(s:= m; SOME a)
						end
		fun items s = NodeSet.addList(NodeSet.empty, !s) 
	end

	val K = 9 (* 9 temp reg's in MIPS *)

	
	fun color {interference as L.IGRAPH{graph=ig, tnode, gtemp, moves}, initial, spillCost, registers} =
		let

			(*val (L.IGraph{graph = ig,
			   tnode = tnode,
			   gtemp = gtemp,	
			   moves = moves}, node2tmps) = L.interferenceGraph(#1 (Makegraph.instr2graph assemlist))*)

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
					val (precolored, initials) = let
						fun f (n, (pre, ini)) = 
							let 
								val tmp = gtemp n 
							in
								if isSome(Temp.look(MipsFrame.tempMap, tmp))
								then (NodeSet.add(pre, n), ini)
								else (pre, NodeSet.add(ini, n))
							end
						in
							foldl f (NodeSet.empty, NodeSet.empty) nodes 
						end
					fun addedge (node, s) = 
						let 
							val adjs = L.IGraph.adj node 
							fun addoneedge (adj, s') = 
								let 
									val e1 = (node, adj)
									val e2 = (adj, node)
								in 
									if MoveSet.member(s', e1) then s' else MoveSet.add(MoveSet.add(s', e1), e2)
								end
						in 
							foldl addoneedge s adjs
						end
					
					val adjSet = foldl addedge MoveSet.empty nodes
				in
					(degreeMap, moveLists, adjSet, adjList, precolored, initials)
				end
				
			val (degreeMap, moveLists, adjSet, adjList, precolored, initials) = build()
			val adjSet = ref adjSet
			val spilledNodes = ref (NodeSet.empty)
			val coalescedNodes = ref (NodeSet.empty)
			val coloredNodes = ref (NodeSet.empty)
			val colorMap = ref (Temp.Table.empty): allocation ref
			val selectStack = Stack.empty
			
			val alias = ref (NodeMap.empty)
			val realias = ref (NodeMap.empty)
			
			fun NodeMoves node = case NodeMap.find(!realias, node) of NONE => MoveSet.intersection(valOf(NodeMap.find(moveLists, node)), MoveSet.union(!worklistMoves, !activeMoves))
																| SOME m =>MoveSet.union(NodeMoves m, MoveSet.intersection(valOf(NodeMap.find(moveLists, node)), MoveSet.union(!worklistMoves, !activeMoves)))
				
			fun MoveRelated node = MoveSet.isEmpty (NodeMoves node)	
			
			fun AddEdge (u, v) = 
				if MoveSet.member(!adjSet, (u, v)) andalso L.IGraph.eq(u,v)
				then ()
				else (adjSet := MoveSet.add(MoveSet.add(!adjSet, (v, u)), (u, v));
						case (NodeSet.member(precolored, u), NodeSet.member(precolored, v)) 
						of (true, true) => ()
						| (false, true) => (valOf(NodeMap.find(adjList, u)) := NodeSet.add(!(valOf(NodeMap.find(adjList, u))), v); 
											valOf(NodeMap.find(degreeMap, u)) := !(valOf(NodeMap.find(degreeMap, u)))+1)
						| (true, false) => (valOf(NodeMap.find(adjList, v)) := NodeSet.add(!(valOf(NodeMap.find(adjList, v))), u);
											valOf(NodeMap.find(degreeMap, v)) := !(valOf(NodeMap.find(degreeMap, v)))+1)
						| (false, false) => (valOf(NodeMap.find(adjList, u)) := NodeSet.add(!(valOf(NodeMap.find(adjList, u))), v);
											valOf(NodeMap.find(degreeMap, u)) := !(valOf(NodeMap.find(degreeMap, u)))+1;
											valOf(NodeMap.find(adjList, v)) := NodeSet.add(!(valOf(NodeMap.find(adjList, v))), u);
											valOf(NodeMap.find(degreeMap, v)) := !(valOf(NodeMap.find(degreeMap, v)))+1)
						)
						
						
			
			fun Adjacent n = NodeSet.listItems (NodeSet.difference(!(valOf(NodeMap.find(adjList, n))), NodeSet.union(Stack.items selectStack, !coalescedNodes)))			
			
			
			fun EnableMoves nodes = 
				let
					fun enable (node, _) =  
						MoveSet.foldl (fn (m, ()) => if MoveSet.member(!activeMoves, m) then (activeMoves := MoveSet.delete(!activeMoves, m); (* FIXME changed comman to semicolon, changed to MoveSet.foldl *)
															worklistMoves := MoveSet.add(!worklistMoves, m)) else ()) () (NodeMoves node) 
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
					
			
			fun MakeWorklist initials =
				let 
					val items = NodeSet.listItems initials
					fun insert (node, ()) = 
							if !(valOf(NodeMap.find(degreeMap, node))) >= K
							then spillWorklist := NodeSet.add(!spillWorklist, node)
							else (
								if MoveRelated node
								then freezeWorklist := NodeSet.add(!freezeWorklist, node)
								else simplifyWorklist := NodeSet.add(!simplifyWorklist, node)
								)
				in
					foldl insert () items
				end
			
			fun Simplify () =
				let 
					val n = NodeSet.listItems(!simplifyWorklist)
					fun sim node = (* FIXME Remove unit from function def *)
						(simplifyWorklist:=NodeSet.delete(!simplifyWorklist, node);
						 Stack.push(node, selectStack);
						 map DecrementDegree (Adjacent node))
				in
					sim (hd n)
				end
				
			(* coalesce **********************************************)
			
			fun GetAlias n =
				if NodeSet.member(!coalescedNodes, n) (* FIXME add n to member check*) 
				then GetAlias(valOf(NodeMap.find(!alias, n)))
				else n
				
			fun OK (t, r) = !(valOf(NodeMap.find(degreeMap, t))) < K orelse 
									NodeSet.member(precolored, t) orelse
									MoveSet.member(!adjSet, (t,r))
									
			fun Conservative nodes =
				let val k = ref 0
					fun judge (node, ()) = if !(valOf(NodeMap.find(degreeMap, node))) >= K then k:=(!k)+1 else ()
				in (foldl judge () nodes;
					!k<K)
				end
			
			fun AddWorkList u =
				if (not (NodeSet.member(precolored, u))) andalso(not (MoveRelated u)) andalso !(valOf(NodeMap.find(degreeMap,u)))<K
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
					 if !(valOf(NodeMap.find(degreeMap, u))) >= K andalso NodeSet.member(!freezeWorklist, u)
					 then (freezeWorklist:=NodeSet.delete(!freezeWorklist, u);
						   spillWorklist:=NodeSet.add(!spillWorklist, u))
					 else ()
					 )
				 end
			
			fun Coalesce () =
				let
					val moveedges = MoveSet.listItems (!worklistMoves)
					fun divide (move as (m, n)) =
						let
							val x = GetAlias m
							val y = GetAlias n
							val (u, v) = if NodeSet.member(precolored, y) then (y, x) else (x, y)
						in
							(worklistMoves := MoveSet.delete(!worklistMoves, move);
							if L.IGraph.eq(u,v)
							then (coalescedMoves:=MoveSet.add(!coalescedMoves, move);
								  AddWorkList u)
							else (
								  if NodeSet.member(precolored, v) orelse MoveSet.member(!adjSet, (u,v))
								  then (constrainedMoves:=MoveSet.add(!constrainedMoves, move);
										AddWorkList u;
										AddWorkList v)
								  else (if (NodeSet.member(precolored, u) andalso (foldl (fn (a, b) => b andalso OK(a, u)) true (Adjacent v)))
											orelse ((not (NodeSet.member(precolored, u))) andalso Conservative((Adjacent u)@(Adjacent v)))
										then (coalescedMoves:=MoveSet.add(!coalescedMoves, move);
											  Combine(u, v);
											  AddWorkList u)
										else (activeMoves:=MoveSet.add(!activeMoves, move)))))
						end
				in
					divide (hd moveedges)
				end
							
			(* freeze **********************************************)
			
			fun FreezeMoves u =
				let 
					val mnodes = MoveSet.listItems (NodeMoves u)
					fun fre (m as (x,y)) = 
						let 
							val v = if L.IGraph.eq((GetAlias y), (GetAlias u)) then (GetAlias x) else (GetAlias y) 
						in
							activeMoves:=MoveSet.delete(!activeMoves, m);
							frozenMoves:=MoveSet.add(!frozenMoves, m);
							if MoveRelated(v) andalso !(valOf(NodeMap.find(degreeMap, v))) < K
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
					val m = #1 (foldr (fn (n, c) => let val sc = spillCost n in if sc < #2 c then (n, sc) else c end) ((hd  l), spillCost (hd l)) (tl l))
				in 
					spillWorklist:=NodeSet.delete(!spillWorklist, m);
					simplifyWorklist:=NodeSet.add(!simplifyWorklist, m);
					FreezeMoves m
				end
			
			
			
			(****************)
			fun repeat () = 
				if not (NodeSet.isEmpty(!simplifyWorklist)) then (Simplify();repeat())
				else if not (MoveSet.isEmpty(!worklistMoves)) then (Coalesce();repeat())
				else if not (NodeSet.isEmpty(!freezeWorklist)) then (Freeze();repeat())
				else if not (NodeSet.isEmpty(!spillWorklist)) then (SelectSpill();repeat())
				else ()

			(**** COLORING ****)


			fun tryColoring() = (case (Stack.pop selectStack) of
					NONE => 
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
				|	SOME node => 
						let
							val adj' = L.IGraph.adj node
							val allRegisters = RegSet.addList(RegSet.empty, registers)
							fun f (node, availColors) = 
								let
									val tmp = gtemp node
									val node' = GetAlias node
									val tmp' = gtemp node'
									val allColoredNodes = NodeSet.union(!coloredNodes, precolored)
								in
									if NodeSet.member(allColoredNodes, node') then
										let
											val c = Temp.Table.look(!colorMap, tmp')
										in
											if isSome c andalso RegSet.member(availColors, valOf c) then
												RegSet.delete(availColors, valOf c)
											else raise ErrorMsg.Error 
										end
									else availColors
								end

							val availColors = foldr f allRegisters adj'
						in
							
							(if RegSet.isEmpty (availColors) then
								spilledNodes := NodeSet.add(!spilledNodes, node)
							else (
								coloredNodes := NodeSet.add(!coloredNodes, node);
								colorMap := Temp.Table.enter(!colorMap, gtemp node, (hd (RegSet.listItems availColors)))
								));
							tryColoring()
						end
				)
		in
			build();
			MakeWorklist initials; 
			repeat();
			(tryColoring(), map gtemp (NodeSet.listItems (!spilledNodes)))
		end
end
