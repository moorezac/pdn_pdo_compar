singler_anno_func <- function(test_list, ref_list, test_hvgs, ref_hvgs, per_cluster = FALSE, cluster_label = "cluster") {
  # generalise
  combinations <- crossing(
    names(test_list),
    names(ref_list)
  ) |>
    rename(test = 1, ref = 2)

  # justin
  gc()

  # could make this faster if we wanted?
  pred <- pmap(
    .progress = TRUE,
    .l = list(
      test = combinations$test,
      ref = combinations$ref
    ),
    .f = function(test, ref) {
      # intersect genes
      test_sce <- test_list[[test]]
      ref_sce <- ref_list[[ref]]

      gene_intersect <- intersect(
        x = rownames(test_sce),
        y = rownames(ref_sce)
      )
      # intersect hvgs
      test_hvgs_fin <- test_hvgs[[test]]
      ref_hvgs_fin <- ref_hvgs[[ref]]
      intersect_hvgs <- intersect(
        x = test_hvgs_fin,
        y = ref_hvgs_fin
      )

      anno <- case_when(
        ref == "couturier" ~ "cluster",
        ref == "ruiz_moreno" ~ "annotation_level_3"
      )

      # run
      if (per_cluster == TRUE) {
        res <- SingleR::SingleR(
          test = test_sce,
          ref = ref_sce,
          restrict = intersect_hvgs,
          labels = colData(ref_sce)[, anno],
          de.method = "wilcox",
          clusters = colData(test_sce)[, cluster_label],
          # gotta go fast
          BPPARAM = MulticoreParam(
            workers = length(parallelly::availableWorkers())
          )
        )
      } else {
        res <- SingleR::SingleR(
          test = test_sce,
          ref = ref_sce,
          restrict = intersect_hvgs,
          labels = colData(ref_sce)[, anno],
          de.method = "wilcox",
          # clusters = colData(test_sce)[, "cluster"],
          # gotta go fast
          BPPARAM = MulticoreParam(
            workers = length(parallelly::availableWorkers())
          )
        )
      }
      res
    }
  )

  if (per_cluster == TRUE) {
    rownames <- "cluster"
  } else {
    rownames <- "bc_wells"
  }
  
  # return
  combinations |> 
    mutate(
      pred = map(
        .x = pred,
        .f = as_tibble, rownames = !!rownames
      )
    )
}
