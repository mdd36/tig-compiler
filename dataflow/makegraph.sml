functor MakeGraph(K:ORD_KEY) :> GRAPH_FUNS where type node_id = K.ord_key =
struct

    structure NodeKey = K
    type node_id = Key.ord_key
    structure NodeMap = RedBlackMapFn(NodeKey)
    structure NodeSet = RedBlackSetFn(NodeKey)
	structure A = Assem
    structure EdgeKey =
    struct
        type ord_key = {src: node_id, dst: node_id}
        fun compare({src=s, dst=d}, {src=s', dst=d'}) =
            case NodeKey.compare(s,s') of
                EQUAL => NodeKey.compare(d,d')
            |   _     => _
    end
    structure EdgeSet = RedBlackSetFn(EdgeKey)

    type 'a node  = (node_id * NodeSet.set * NodeSet.set)
    type 'a graph = 'a node NodeMap.map
    type 'a edge  = {src: node_id, dst: node_id}

    val empty = NodeMap.empty
	val edgeset = 
    fun empty_node(node_id', dat) = (node_id', dat, NodeSet.empty, NodeSet.empty)
    fun mk_node(graph', node_id', dat) =
        NodeMap.insert(graph', node_id', empty_node(node_id', dat))
    fun mk_edge(graph', {src,dst}) = graph' (*TODO*)
    fun rm_edge(graph', {src,dst}) = graph' (*TODO*)
	
	
	fun genter (g, k, tl) = foldl (fn (a, g') => Graph.Table.enter(g', k, a)) g tl 
	fun instr2graph assemlist = 
			let 
				fun instr (A.MOVE{assem, dst, src}, {control = g, def = deft, use = uset, ismove = mt}) = 
						if dst = src 
							then {control = g, def = deft, use = uset, ismove = mt}
							else (let val newnode = Graph.newNode g
									in {control = g, def = genter(deft, newnode, [dst]), use = genter(uset, newnode, [src]), ismove =genter(mt, newnode, [true])}
									end)
				
				  | instr (A.LABEL{assem, lab}, {control = g, def = deft, use = uset, ismove = mt}) = 
				  
				  | instr (A.OPER{assem, dst, src, jump}, {control = g, def = deft, use = uset, ismove = mt}) = if 
				  
			in
			  foldl instr Flow.FGRAPH{control = Graph.newGraph(),
				    def = Graph.Table.empty,
				    use = Graph.Table.empty,
				    ismove = Graph.Table.empty} assemlist 

    
end
