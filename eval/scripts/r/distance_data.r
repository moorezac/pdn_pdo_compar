# this went from ~ 4 hours to 30s
# how to write a rds file faster???
distance_data <- imap(
  .progress = TRUE,
  .x = sce_list,
  .f = function(sce, i) {
    # setup
    library(igraph)
    library(parallel)
    avail_cores <- parallelly::availableWorkers() |> length()

    # create graph from scvi dimreds
    g <- scran::buildSNNGraph(
      x = sce,
      use.dimred = "X_scVI",
      type = "jaccard",
      BNPARAM = BiocNeighbors::HnswParam()
    )

    # setup
    barcodes_full <- colData(sce) |> 
      as_tibble() |> 
      select(bc_wells, sample) |> 
      mutate(index = 1:n())
    barcodes <- c("t", "pdn", "pdo") |> 
      set_names() |> 
      map(
        .f = function(x){
          barcodes_full |>
            filter(
              str_detect(
                string = sample,
                pattern = fixed(pattern = x, ignore_case = TRUE)
              )
            ) |>
            pull(index)
        }
      )
    
    # these was the time savers
    # 1 - the overhead for calling function too great
    # 2 - working with indices and not character names
    
    # calculate at start
    edge_weights <- igraph::edge_attr(graph = g, name = "weight")
    
    distances_all <- mclapply(
      mc.cores = avail_cores - 1,
      X = barcodes$t,
      FUN = function(x) {
        shortest_paths <- igraph::shortest_paths(
          graph = g,
          from = igraph::V(g)[x],
          to = c(barcodes$pdn, barcodes$pdo),
          output = "both"
        )

        distance_node <- lapply(
          X = shortest_paths$vpath,
          FUN = length
        )
        names(distance_node) <- c(barcodes$pdn, barcodes$pdo)
        
        distance_length <- lapply(
          X = shortest_paths$epath,
          FUN = function(x) {
            edge_weights[as.numeric(x)] |> 
              sum()
          }
        )
        names(distance_length) <- c(barcodes$pdn, barcodes$pdo)

        bc_wells <- barcodes_full$bc_wells
        
        distances <- tibble(
          from = bc_wells[x],
          to = c(bc_wells[barcodes$pdn], bc_wells[barcodes$pdo]),
          distance_length = unlist(distance_length),
          distance_node = unlist(distance_node)
        )

        distances
      }
    )
    
    names(distances_all) <- barcodes$t
    
    write_rds(
      x = distances_all,
      file = here("data", "processed", "2_2_distances", paste(i, "distances.rds", sep = "_"))
    )
    write_rds(
      x = g,
      file = here("data", "processed", "2_2_distances", paste(i, "snn_graph.rds", sep = "_"))
    )
    
  bind_rows(distances_all)
  
  }
)
