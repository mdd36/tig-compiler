functor MakeGraph(K:ORD_KEY) :> GRAPH_FUNS where type node_id = K.ord_key =
struct

    structure NodeKey = K
    type node_id = Key.ord_key
    structure NodeMap = RedBlackMapFn(NodeKey)
    structure NodeSet = RedBlackSetFn(NodeKey)

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

    fun empty_node(node_id', dat) = (node_id', dat, NodeSet.empty, NodeSet.empty)
    fun mk_node(graph', node_id', dat) =
        NodeMap.insert(graph', node_id', empty_node(node_id', dat))
    fun mk_edge(graph', {src,dst}) = graph' (*TODO*)
    fun rm_edge(graph', {src,dst}) = graph' (*TODO*)

    
end
