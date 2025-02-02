load_ref_list <- function() {
  list(
    readRDS("/stornext/Bioinf/data/lab_brain_cancer/public_datasets/sc_gbm_big/sce_downsampled.rds"),
    readRDS("/stornext/Bioinf/data/lab_brain_cancer/public_datasets/Deconvolution References/Couturier_dataset_tumor_lognormalised.rds")
  ) |> 
    set_names(
      c(
        "ruiz_moreno",
        "couturier"
      )
    )
}
