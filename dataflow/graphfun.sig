signature GRAPH_FUNS =
sig
    val instr2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
    type node_id
    type 'a edge = {src: node_id, dst: node_id}
    type 'a graph
    type 'a node

    val empty : 'a graph
    val mk_node : 'a graph * node_id * 'a -> 'a graph
    val mk_edge : 'a graph * 'a edge -> 'a graph
    val rm_edge : 'a graph * 'a edge -> 'a graph
    val rm_node : 'a graph * node_id -> 'a graph
    val find_node : 'a graph * node_id -> 'a node
    val replace_node : 'a graph * node_id * 'a -> 'a graph
    val succ_by_ID : 'a graph -> node_id -> node_id list
    val succ_by_node : 'a graph -> node_id -> node_id list
    val pred_by_ID : 'a graph -> node_id -> node_id list
    val pred_by_node : 'a graph -> node_id -> node_id list

end
