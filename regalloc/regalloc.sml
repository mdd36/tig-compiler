signature REG_ALLOC =
sig 
	structure Frame: FRAME
	type allocation = MipsFrame.register Temp.Table.table
	val alloc : Assem.instr list * MipsFrame.frame *bool ->
							Assem.instr list * allocation 
end

structure Regalloc :> REG_ALLOC =
struct	
	structure Frame = MipsFrame
	structure A = Assem
	structure RD = Reachingdefinition
	type allocation = Frame.register Temp.Table.table
	open Util
	structure NodeSet = RedBlackSetFn(type ord_key=Liveness.IGraph.node val compare=Liveness.IGraph.compare)
	val spilled = ref (NodeSet.empty)
	
	fun rewriteProg(assemlist, frame, spillList) = 
		let
		 	fun singleSpill (tmp, assemlist') = 
		 		let
		 			val newAccess = Frame.find(Frame.allocLocal(frame)(true))(Tree.TEMP Frame.FP)
		 			
		 			fun allocNewReg(true, assem, oldTemp) = 
		 				if contains(assem, oldTemp) then 
		 					let
		 						val newTemp = Temp.newtemp()
		 					in
		 						(Mipsgen.codegen(frame)(Tree.MOVE(newAccess, Tree.TEMP newTemp)), (*sw*)
		 							map (replace oldTemp newTemp) assem)
		 					end
		 				else ([],assem)
	 				| 	allocNewReg(false, assem, oldTemp) = 
		 					if contains(assem, oldTemp) then 
			 					let
			 						val newTemp = Temp.newtemp()
			 					in
			 						(Mipsgen.codegen(frame)(Tree.MOVE(Tree.TEMP newTemp, newAccess)), (*lw*)
			 							map (replace oldTemp newTemp) assem)
			 					end
			 				else ([],assem)

			 		fun singleInstr (A.OPER{assem=assem, dst=dst, src=src, jump=jump}) = 
			 				let
			 					val loads  = allocNewReg(false, src, tmp)
			 					val stores = allocNewReg(true,  dst, tmp)
			 					
			 					val lwCode = #1 loads
			 					val src' = #2 loads
			 					val swCode = #1 stores
			 					val dst' = #2 stores
			 				in
			 					lwCode @ [A.OPER{assem=assem, dst=dst', src=src', jump=jump}] @ swCode
			 				end
			 		|	singleInstr (A.MOVE{assem=assem, dst=dst, src=src}) = 
			 				let
			 					val loads  = allocNewReg(false, [src], tmp)
			 					val stores = allocNewReg(true,  [dst], tmp)
			 					
			 					val lwCode = #1 loads
			 					val src' = [hd (#2 loads)]
			 					val swCode = #1 stores
			 					val dst' = [hd (#2 stores)]
			 				in
			 					lwCode @ [A.OPER{assem=assem, dst=dst', src=src', jump=NONE}] @ swCode
			 				end
			 		| 	singleInstr x = x :: []

			 		fun f singleInstr (instr, rest) = (singleInstr instr) @ rest

		 		in
		 			foldr (f singleInstr) [] assemlist'
		 		end
		 in
		 	foldr singleSpill assemlist spillList
		 end 
	
	fun isequal ([], []) = true
	|   isequal (a, []) = false
	|   isequal ([], a) = false
	|   isequal (a1::l1, a2::l2) = if A.equal(a1,a2) then isequal(l1,l2) else false
	
	fun dfanalysis assemlist =
		let
			val (graph as Flow.FGRAPH{control=control, def=deft, use=uset, ismove=ism}, nodes, getdefs, assem2node) = MakeGraph.instr2graph assemlist
			val (getRDin, getRDout) = RD.RDanalysis(graph, nodes, getdefs)
			val assems = RD.propagation(assemlist, assem2node, graph, getRDin)
			fun eliminate al = 
				let 
					val (graph as Flow.FGRAPH{control=control, def=deft, use=uset, ismove=ism}, nodes, getdefs, assem2node) = MakeGraph.instr2graph al
					val (igraph, liveOut) = Liveness.interferenceGraph graph
				in
					RD.eliminatedeadcode (al, assem2node, graph, liveOut) 
				end				
		in 
			(*if isequal(assems, assemlist) then eliminate assems else dfanalysis assems*)
			eliminate assems
		end
			
		 
	fun alloc (assemlist', frame, b) =
		let
			val assemlist = dfanalysis assemlist'
			val (graph as Flow.FGRAPH{control=control, def=deft, use=uset, ismove=ism}, nodes, getdefs, assem2node) = MakeGraph.instr2graph assemlist
			val (igraph, liveOut) = Liveness.interferenceGraph graph
			val Liveness.IGRAPH{graph=graph', tnode=tnode, gtemp=gtemp, moves=moves} = igraph
			val _ = (if b then spilled:=(NodeSet.empty) else ())

			fun spillcost (node:Liveness.IGraph.node) = (* Spill cost is num defs + num uses *)
				let
					val tmp = gtemp node
					(* Head since we're now getting a list of list *)
					fun f (g, x) = 
						let
							val def' = (case (Flow.Graph.Table.look(deft, g)) of NONE => []
																		| SOME d => d)
							val use' = (case (Flow.Graph.Table.look(uset, g)) of NONE => []
																		| SOME d => d)
						in
							if (NodeSet.member(!spilled, node)) then Real.posInf
							else x + (Real.fromInt((if contains(def', tmp) then 1 else 0) + (if contains(use', tmp) then 1 else 0)) / Real.fromInt(length (Liveness.IGraph.adj node)))
						end
					in
						foldr f 0.0 nodes
				end

			val (allocation, spillList) = Color.color {interference=igraph, 
													   initial=(Frame.tempMap : allocation), 
													   spillCost=spillcost, 
													   registers=Frame.registerColors()}


			fun removeStupid ((a as Assem.MOVE{dst,src,...})::l) = 
					(case String.compare(valOf(Temp.Table.look(allocation, src)), 
										 valOf(Temp.Table.look(allocation, dst))) of
						EQUAL => removeStupid l
					|	_ =>  a :: removeStupid l)
			|	removeStupid ((a as Assem.OPER{assem, ...}) :: (a' as Assem.OPER{assem=assem',...}) :: l) = 
					let
						val fmt = Assem.format(Frame.makestring2 allocation)
						val sameMem = 
							let
								val swLocation = String.extract(fmt a, 3, NONE) (* if sw source = lw dst && &sw = &lw then markAsStupid() *)
								handle Subscript => "b\n"
								val lwLocation = String.extract(fmt a', 3, NONE)
								handle Subscript => "a\n"
							in
								case String.compare(swLocation, lwLocation) of
									EQUAL => true
								|	_ => false

							end
					in
						if String.isPrefix "sw" assem andalso String.isPrefix "lw" assem' andalso sameMem then a :: removeStupid l
						(*else if String.isPrefix "sw" assem andalso String.isPrefix "move" assem' andalso sameReg then Assem*)
						else a ::  removeStupid (a' :: l)
					end
			|	removeStupid (a::l) = a :: removeStupid l
			|	removeStupid ([]) = []		
		in
			if length spillList = 0 then (removeStupid assemlist, allocation)

			else (spilled:= NodeSet.addList(!spilled, map tnode spillList);
				 alloc(rewriteProg(assemlist, frame, spillList), frame, false))
		end
	 

	


end
