signature REG_ALLOC =
sig 
	structure Frame: FRAME
	type allocation = MipsFrame.register Temp.Table.table
	val alloc : Assem.instr list * MipsFrame.frame ->
							Assem.instr list * allocation
end

structure Regalloc :> REG_ALLOC =
struct	
	structure Frame = MipsFrame
	structure A = Assem
	type allocation = Frame.register Temp.Table.table
	open Util
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
		 
	fun alloc (assemlist, frame) =
		let
			val format1 = Assem.format(Frame.makestring)
			val (graph as Flow.FGRAPH{control, def, use, ismove}, nodes) = MakeGraph.instr2graph assemlist
			val (igraph, liveOut) = Liveness.interferenceGraph graph
			val d' as Liveness.IGRAPH{graph=graph', tnode, gtemp, moves} = igraph


			fun spillcost (node:Liveness.IGraph.node) = (* Spill cost is num defs + num uses *)
				let
					val tmp = gtemp node
					(* Head since we're now getting a list of list *)
					fun f (g, x) = 
						let
							val def' = valOf(Graph.Table.look(def, g))
							val use' = valOf(Graph.Table.look(use, g))
						in
							x + (if contains(def', tmp) then 1 else 0) + (if contains(use', tmp) then 1 else 0)
						end
					in
					foldr f 0 nodes 
				end

			val (allocation, spillList) = Color.color {interference=igraph, 
													   initial=(Frame.tempMap : allocation), 
													   spillCost=spillcost, 
													   registers=Frame.registerColors()}
			fun isNotStupidMove (a as Assem.MOVE{dst,src,...}) = (* idk why it was happening but this fixes the move v0 v0\n move v0 v0 that's in a lot of our funcs*)
					valOf(Temp.Table.look(allocation, src)) <> valOf(Temp.Table.look(allocation, dst))
			|	isNotStupidMove x = true
		in
			if length spillList = 0 then (List.filter isNotStupidMove assemlist, allocation)
			else alloc(rewriteProg(assemlist, frame, spillList), frame)
		end
	 

	


end
