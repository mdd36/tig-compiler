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

			 		fun f singleInstr (instr, rest) = rest @ singleInstr instr

		 		in
		 			foldr (f singleInstr) [] assemlist'
		 		end
		 in
		 	foldr singleSpill assemlist spillList
		 end 
		 
	fun alloc (assemlist, frame) =
		let
			fun spillcost (node:Liveness.IGraph.node) = 1:int (* TODO better spill cost *)

			val format1 = Assem.format(Frame.makestring)
			val graph = #1(MakeGraph.instr2graph assemlist)
			val (igraph, lo) = Liveness.interferenceGraph graph

			val (allocation, spillList) = Color.color {interference=igraph, 
													   initial=(Frame.tempMap : allocation), 
													   spillCost=spillcost, 
													   registers=Frame.registerColors()}
		in
			if length spillList = 0 then (assemlist, allocation)
			else alloc(rewriteProg(assemlist, frame, spillList), frame)
		end
	 

	


end
