signature LIVENESS:
sig
	structure IGraph
	datatype igraph = 
		IGRAPH of {graph: IGraph.graph,
				   tnode: Temp.temp -> IGraph.node,
				   gtemp: IGraph.node -> Temp.temp,
				   moves: (IGraph.node * IGraph.node) list}
	
	val interferenceGraph :
			Flow.flowgraph ->
				igraph * (Flow.Graph.node -> Temp.temp list)
				
	val show : outstream * igraph -> unit
end

structure Liveness :> LIVENESS =
struct
	structure IGraph = Graph
	structure TempSet = RedBlackSetFn(type ord_key=Temp.temp val compare=Int.compare)
	structure NodeMap = RedBlackMapFn(type ord_key=Flow.Graph.node val compare=Flow.Graph.compare) (*node -> livein set * liveout set*)
	structure NtMap = RedBlackMapFn(type ord_key=IGraph.node val compare=IGraph.compare) (*node -> temp*)
	structure TnMap = RedBlackMapFn(type ord_key=Temp.temp val compare=Int.compare) (*temp -> node*)
	structure EdgeKey =
    struct
        type ord_key = {src: IGraph.node, dst: IGraph.node}
        fun compare({src=s, dst=d}, {src=s', dst=d'}) =
            (case IGraph.compare(s,s') of
                EQUAL => IGraph.compare(d,d')
                | LESS => LESS
				| GREATER => GREATER)
    end
	
    structure EdgeSet = RedBlackSetFn(EdgeKey)
	
	fun getStart (node, mymap) = NodeMap.insert(mymap, node, (TempSet.empty, TempSet.empty))
	
	fun getLive (b, mymap, deft, uset, nodea::nodes) = let 
														val defs = valOf(Flow.Graph.Table.look(deft, nodea)) 
														val uses = valOf(Flow.Graph.Table.look(uset, nodea))
														val defset = TempSet.addList(TempSet.empty, defs)
														val useset = TempSet.addList(TempSet.empty, uses)
														val (livein', liveout') = valOf(NodeMap.find(mymap, nodea))
														val livein = TempSet.union(useset, TempSet.difference(liveout', defset))
														val succs = Flow.Graph.succ nodea
														val liveout = foldl (fn (a,b) => TempSet.union(b, #1 (valOf(NodeMap.find(mymap, a))))) TempSet.empty succs
														
													in
														getLive(b andalso (livein = livein') andalso (liveout = liveout'),
																NodeMap.insert(mymap, node, (livein, liveout)), deft, uset, nodes)
													end
	  | getLive (b, mymap, deft, uset, nil) = (b, mymap)
		
	
	fun iteration (b, mymap, deft, uset, nodes) = let
													val (b', map') = getLive (b, mymap, deft, uset, nodes)
												  in
													if b' then map' else iteration (true, map', deft, uset, nodes)
												  end
												  
	fun interferenceGraph (fg as Flow.FGRAPH{control = cfg,def = deft, use = uset, ismove = imt}) =
			let
				val nodes = Flow.Graph.nodes cfg
				val mymap = iteration(true, foldl getStart NodeMap.empty nodes, deft, uset, rev nodes)
				
				val ig = IGraph.newGraph();			
														
				fun getMapping (nodea, (ntm, tnm)) = 
					let 
						val defs = valOf(Flow.Graph.Table.look(deft, nodea)) 
						val uses = valOf(Flow.Graph.Table.look(uset, nodea))
						
						fun addNode (a, (ntm', tnm')) = case TnMap.find(tnm', a) of NONE => (let val newn = IGraph.newNode ig 
																			in (NtMap.insert(ntm', newn, a), TnMap.insert(tnm', a, newn))
																			end)
																| SOME b => (ntm', tnm')
					in 
						foldl addNode (ntm, tnm) (defs@uses)
					end
					
				val (ntmap, tnmap) = foldl getMapping (NtMap.empty, TnMap.empty) nodes
																			
				fun addEdge (node, eset) = 
						let
							val LOs = listItems(#2 (valOf(NodeMap.find(mymap, node))))
							val defs = valOf(Flow.Graph.Table.look(deft, nodea))
							fun addOneDef (tmp, ese) = 
								let 
									val mynode = valOf(TnMap.find(tnmap, tmp))
									fun addOneLO (lotmp, ese') =
										let 
											val lonode = valOf(TnMap.find(tnmap, lotmp))
											val edge = {from = mynode, to = lonode}
											val edge' ={from = lonode, to = mynode}
										in
											if EdgeSet.member(ese', edge) then ese'
											else (IGraph.mk_edge edge; EdgeSet.add(EdgeSet.add(ese', edge),edge'))
										end
								in
									foldl addOneLO ese LOs
								end
						in
							foldl addOneDef eset defs
						end
						
				val _ = foldl addEdge EdgeSet.empty nodes
				
				fun getMoves (node, l) = if valOf(Flow.Graph.Table.look(imt, node)) 
											then (hd (valOf(Flow.Graph.Table.look(deft, nodea))), hd (valOf(Flow.Graph.Table.look(uset, nodea))))::l
											else l
				val moves = foldl getMoves [] nodes
				
				fun tnode tmp = case TnMap.find(tnmap, tmp) of NONE => (print("ERROR: No such temp found: t"^Int.toString tmp^"\n"); IGraph.errorNode ig)
														| SOME node => node
				
				fun gtemp nde = case NtMap.find(ntmap, nde) of NONE => (print("ERROR: No such node found: "^IGraph.nodename nde^"\n"); ~1: Temp.temp)
														 | SOME tmp => tmp
				
				fun getLO node = case NodeMap.find(mymap, node) of NONE => (print("ERROR: No such node found: "^IGraph.nodename node^"\n"; [])
																| SOME tmps => tmps
			in 
				(IGRAPH{graph = ig,
					   tnode = tnode,
					   gtemp = gtemp,
					   moves = moves}, getLO)
			end
												  
	fun withOpenFile fname f =
		   let val out = TextIO.openOut fname
		   in (f out before TextIO.closeOut out)
			handle e => (TextIO.closeOut out; raise e)
		   end

    
        
	fun show (outs, ig) = 
			let 
				val nodes = IGraph.nodes ig
				
				fun emit out node = 
					let 
						val adjs = IGraph.adj node 
						fun emitadjs a::m::l = (IGraph.nodename a)^ ", " ^ emitadjs(m::l)
						  | emitadjs a::m = IGraph.nodename a
						  | emitadjs nil = ""
					in
					
						TextIO.output(out, IGraph.nodename node^": "^ emitadjs adjs ^"\n")
					end
			in

                withOpenFile (outs ^ ".txt")  (fn out => (app (emit out) nodes))
			end
	



end