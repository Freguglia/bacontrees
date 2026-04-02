.ct_to_igraph <- function(ct, activeOnly){
  df_tree <- map(ct$nodes, function(x) {
    if(!x$isLeaf()){
      count <- map_int(ct$nodes[ct$nodes[[x$getPath()]]$getChildrenPaths()],
                       function(y) sum(y$counts))
      data.frame(from = x$getPath(), to = x$getChildrenPaths(),
                 n = count)
    } else {
      NULL
    }
  }) |> dplyr::bind_rows()
  df_nodes <- map(ct$nodes, function(x) data.frame(
    vertices = x$getPath(),
    nodeLabel = stringr::str_replace(x$getPath(), ".*\\.", "")
  )) |> dplyr::bind_rows()

  if(activeOnly){
    inc_nodes <- c(ct$getActiveNodes(), ct$getInnerNodes())
    df_tree <- df_tree |> filter(from %in% inc_nodes, to %in% inc_nodes)
    df_nodes <- df_nodes |> filter(vertices %in% inc_nodes)
  }

  node_paths <- df_nodes$vertices
  node_state <- rep("inactive", length(node_paths))
  node_state[node_paths %in% ct$getActiveNodes()] <- "active"
  node_state[node_paths %in% ct$getInnerNodes()] <- "inner"

  edge_paths <- df_tree$to
  edge_state <- rep("inactive", length(edge_paths))
  edge_state[edge_paths %in% ct$getActiveNodes()] <- "active"
  edge_state[edge_paths %in% ct$getInnerNodes()] <- "active"

  gr <- igraph::graph_from_data_frame(df_tree, vertices = df_nodes)
  attr_names <- names(ct$nodes[[1]]$extra)
  nodes_order <- names(igraph::V(gr))
  all_counts <- map(ct$nodes[nodes_order], function(x) x$counts)
  igraph::vertex_attr(gr, "counts") <- all_counts
  igraph::vertex_attr(gr, "state") <- node_state
  igraph::edge_attr(gr, "state") <- edge_state
  for(attr in attr_names){
    attr_values <- map(ct$nodes[nodes_order], function(x) x$extra[[attr]])
    igraph::vertex_attr(gr, attr) <- flatten_if_scalar(attr_values)
  }
  gr
}

flatten_if_scalar <- function(x) {
  if (all(vapply(x, length, integer(1)) == 1L)) {
    unlist(x, recursive = FALSE, use.names = FALSE)
  } else {
    x
  }
}
