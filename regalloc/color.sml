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

	type allocation = string Temp.table
	open Util
	exception NotEnoughColors of string

	fun color {interference as IGRAPH.Liveness{graph, tnode, gtemp, moves}, initial, spillCost, registers} =

		let
			fun isNotPrecolored tmp = 
				let
					val initialName = Temp.Table.look(initial, tmp)
				in
					isSome initialName andalso isSome (List.find(registers, initialName)) 
				end 

			fun iter [] = (initial, [])
			|	iter [node] = 
					let
						val neigborsInTemps = filter (fn tmp => isSome (Temp.table.look(initial, tmp))) (Graph.pred node)
						val neighborColors = foldr (fn (head, res) => getOpt(Temp.table.look(initial, head), "GENERAL KENOBI") :: res) [] neigborsInTemps
						val remainingColors = filter (fn c => not isSome(find(neighborColors, c))) registers
					in
						if length remainingColors > 0 then
							(Temp.Table.enter(initial, gtemp node, hd remainingColors), [])
						else 
							raise NotEnoughColors "Cannot color given graph with given colors"

					end
			|	iter nodes =  
					let
						val trivialNodes = filter (fn n => length (Graph.succ(n)) < length registers) nodes
						val toRemove = if length trivialNodes > 0 then
							hd trivialNodes
						else (
							println("Cannot find a trivial node to remove, a node to coalesce, or a move edge to remove, spilling a node");  TODO check for conditions besides trivial nodes
							#1 (foldr (fn (head, res) => let val cost = spillCost head in if cost < #2 res then (head, cost) else res end) IntInf.int nodes)
						)
						val _ = app Graph.rm_edge Graph.adj(toRemove) (* Remove all the edges as a psuedo-remove from the graph *)

					in
						
					end

			val initals = filter isNotPrecolored Graph.nodes(graph)
		in
			iter initials
		end
end
