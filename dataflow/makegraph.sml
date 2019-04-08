signature MAKEGRAPH =
sig
	val instr2graph: Assem.instr list ->
				Flow.flowgraph * Flow.Graph.node list
end

structure MakeGraph :> MAKEGRAPH=
struct
	structure A = Assem
	(*
    structure NodeKey = K
    type node_id = Key.ord_key
    structure NodeMap = RedBlackMapFn(NodeKey)
    structure NodeSet = RedBlackSetFn(NodeKey)
	
    type 'a node  = (node_id * NodeSet.set * NodeSet.set)
    type 'a graph = 'a node NodeMap.map
    type 'a edge  = {src: node_id, dst: node_id}

    val empty = NodeMap.empty
    fun empty_node(node_id', dat) = (node_id', dat, NodeSet.empty, NodeSet.empty)
    fun mk_node(graph', node_id', dat) =
        NodeMap.insert(graph', node_id', empty_node(node_id', dat))
    fun mk_edge(graph', {src,dst}) = graph' (*TODO*)
    fun rm_edge(graph', {src,dst}) = graph' (*TODO*)
	*)
	
	structure EdgeKey =
    struct
        type ord_key = {from: Flow.Graph.node, to: Flow.Graph.node}
        fun compare({from=s, to=d}, {from=s', to=d'}) =
            (case Flow.Graph.compare(s,s') of
                EQUAL => Flow.Graph.compare(d,d')
                | LESS => LESS
				| GREATER => GREATER)
    end
	
    structure EdgeSet = RedBlackSetFn(EdgeKey)
	structure NodeMap = RedBlackMapFn(type ord_key=Flow.Graph.node val compare=Flow.Graph.compare)
	val edgeset = EdgeSet.empty
	val labtabel =  Symbol.empty  (*Tabel used to store lab -> node (which node has the lab)*)
	val nodemap = NodeMap.empty (*Tabel used to store node -> lab (which node has the jump [labs])*)
	
	
	fun genter (g, k, tl) = foldl (fn (a, g') => case Flow.Graph.Table.look(g', k) of NONE => Flow.Graph.Table.enter(g', k, [a])
																				 | SOME al => Flow.Graph.Table.enter(g', k, a::al)) g tl 
	
	fun instr2graph assemlist = 
			let 
				fun instr (A.MOVE{assem, dst, src}, (oldnode, eset, ltabel, nmap, Flow.FGRAPH{control = g, def = deft, use = uset, ismove = mt})) = 
							if dst = src 
							then (oldnode, eset, ltabel, nmap, Flow.FGRAPH{control = g, def = deft, use = uset, ismove = mt})
							else (let val newnode = Flow.Graph.newNode g
									in 
										(case oldnode of NONE => (SOME newnode, eset, ltabel, nmap, Flow.FGRAPH{control = g, def = genter(deft, newnode, [dst]), use = genter(uset, newnode, [src]), ismove = Flow.Graph.Table.enter(mt, newnode, true)})
												   | SOME old => (let val e = {from=old, to = newnode}
																in																	
																	Flow.Graph.mk_edge(e);
																	(SOME newnode, EdgeSet.add(eset, e), ltabel, nmap, Flow.FGRAPH{control = g, def = genter(deft, newnode, [dst]), use = genter(uset, newnode, [src]), ismove = Flow.Graph.Table.enter(mt, newnode, true)})
																	end))
									end)
				
				  | instr (A.LABEL{assem, lab}, (oldnode, eset, ltabel, nmap, Flow.FGRAPH{control = g, def = deft, use = uset, ismove = mt})) = 
								(let val newnode = Flow.Graph.newNode g
									in (case oldnode of NONE => (SOME newnode, eset, Symbol.enter(ltabel, lab, newnode), nmap, Flow.FGRAPH{control = g, def = deft, use = uset, ismove =Flow.Graph.Table.enter(mt, newnode, false)})
												  | SOME old => (
																let val e = {from=old, to = newnode}
																in	
																Flow.Graph.mk_edge(e);
																(SOME newnode, EdgeSet.add(eset, e), Symbol.enter(ltabel, lab, newnode), nmap, Flow.FGRAPH{control = g, def = deft, use = uset, ismove =Flow.Graph.Table.enter(mt, newnode, false)})
																end))
									end)
				  
				  | instr (A.OPER{assem, dst, src, jump}, (oldnode, eset, ltabel, nmap, Flow.FGRAPH{control = g, def = deft, use = uset, ismove = mt})) = 
								(let val newnode = Flow.Graph.newNode g
									in (case jump of NONE => (case oldnode of NONE => (SOME newnode, eset, ltabel, nmap, Flow.FGRAPH{control = g, def = genter(deft, newnode, dst), use = genter(uset, newnode, src), ismove =Flow.Graph.Table.enter(mt, newnode, false)})
																	   | SOME old => (let val e = {from=old, to = newnode}
																						in	
																						Flow.Graph.mk_edge(e);
																						(SOME newnode, EdgeSet.add(eset, e), ltabel, nmap, Flow.FGRAPH{control = g, def = genter(deft, newnode, dst), use = genter(uset, newnode, src), ismove =Flow.Graph.Table.enter(mt, newnode, false)})
																						end))
											 | SOME llist => (case oldnode of NONE => (SOME newnode, eset, ltabel, NodeMap.insert(nmap, newnode, llist), Flow.FGRAPH{control = g, def = genter(deft, newnode, dst), use = genter(uset, newnode, src), ismove =Flow.Graph.Table.enter(mt, newnode, false)})
																	   | SOME old => (let val e = {from=old, to = newnode}
																						in	
																						Flow.Graph.mk_edge(e);
																						(SOME newnode, EdgeSet.add(eset, e), ltabel, NodeMap.insert(nmap, newnode, llist), Flow.FGRAPH{control = g, def = genter(deft, newnode, dst), use = genter(uset, newnode, src), ismove =Flow.Graph.Table.enter(mt, newnode, false)})
																						end)))
									end)
				
				
				fun addedge (eset, ltabel, nmap) = 
							let 
								val items = NodeMap.listItemsi(nmap)
								fun searchedge (node, a::m) = (case Symbol.look(ltabel, a) of NONE => (print("ERROR: No such label in the program"^Symbol.name a^"\n") ;searchedge(node, m))
																						 | SOME newn => ({from = node, to = newn}::searchedge(node, m)))
								  | searchedge (node, nil) = []
								  
								fun adde (es, (e::el)) = (if EdgeSet.member(es, e) then adde (es, el)
													else (Flow.Graph.mk_edge(e); adde(EdgeSet.add(es, e), el)))
								  | adde (es, nil) = es
							in
								foldl (fn ((a,b),es) => adde(es, searchedge(a,b))) eset items
							end
												
				val (n, eset, ltabel, nmap, fg) = foldl instr (NONE, edgeset, labtabel, nodemap, Flow.FGRAPH{control = Flow.Graph.newGraph(),
																														def = Flow.Graph.Table.empty,
																														use = Flow.Graph.Table.empty,
																														ismove = Flow.Graph.Table.empty}) assemlist 
				val Flow.FGRAPH{control=ctr, def=def, use=use, ismove=ism} = fg  
			in
			   addedge(eset, ltabel, nmap);
			   
			  (fg, Flow.Graph.nodes ctr)
			end

    
end
