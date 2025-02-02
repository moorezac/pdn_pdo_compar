suppressPackageStartupMessages(library(tidyverse))

full_list <- readRDS(file = "/stornext/Bioinf/data/lab_brain_cancer/users/z_moore/full_list.rds")

# size of each set
num_genes <- 50
# extract and format
full_sets <- map(
  .x = names(full_list),
  .f = function(x) {
    tibble(
      !!paste(x, "high", sep = "_") := full_list[[x]] |> 
        head(num_genes) |>
        pull(gene_name),
      !!paste(x, "low", sep = "_") := full_list[[x]] |> 
        tail(num_genes) |>
        pull(gene_name)
    )
  }
) |> 
  bind_cols()

sce <- sce_tumour
colnames(sce) <- colData(sce)$bc_wells
rownames(sce) <- rowData(sce)$gene_name
# example code
library(escape)
# can alter method/sizes
# either AUCell or UCell fast
ssgsea_scores <- escape::escape.matrix(
  input.data = counts(sce),
  gene.sets = as.list(full_sets),
  method = "AUCell",
  # groups = 1000,
  min.size = 5,
  BPPARAM = MulticoreParam(
    workers = length(parallelly::availableWorkers())
  )
)
# add in
colData(sce) <- colData(sce) |> 
  as_tibble() |> 
  left_join(
    y = ssgsea_scores |> as_tibble(rownames = "bc_wells"),
    by = "bc_wells"
    ) |> 
  DataFrame()

# integrated score
# TODO is this bueno?
ssgsea_norm_scores <- map(
  .x = colnames(ssgsea_scores) |> 
  str_split(pattern = "_") |> 
  map_vec(1) |> 
  unlist() |> 
  unique(),
  .f = function(x) {
    ssgsea_scores |> 
      as_tibble(rownames = "bc_wells") |> 
      select("bc_wells", contains(x)) %>% 
      # high before low
      mutate(!!paste(x, "norm", sep = "_") := .[[2]] - .[[3]]) |> 
      select(1, 4)
  }
) |> 
  # justin
  purrr::reduce(left_join, by = join_by("bc_wells"))

colData(sce) <- colData(sce) |> 
  as_tibble() |> 
  left_join(
    y = ssgsea_norm_scores |> as_tibble(),
    by = "bc_wells"
  ) |> 
  DataFrame()


plot_ssgsea <- ssgsea_norm_scores |> 
  pivot_longer(cols = -bc_wells) |> 
  left_join(y = colData(sce) |> as_tibble()) |> 
  # group_by(name, sample) |> 
  # summarise(mean = mean(value)) |> 
  # ungroup() |> 
  # mutate(line = str_sub(sample, start = 8))
  mutate(Sample = type) |> 
  mutate(Sample = case_when(Sample == "T" ~"TIS", .default = Sample)) |> 
  filter(line != "GL0128") |> 
  mutate(
    name = case_when(
      name == "astro_norm" ~ "AC-like",
      name == "mes_norm" ~ "MES-like",
      name == "oligo_norm" ~ "OPC-like",
      name == "neuro_norm" ~ "NPC-like",
      name == "inter_norm" ~ "Progenitor",
    )
    ) |> 
  ggplot(
    mapping = aes(
      x = value,
      y = name,
      fill = Sample
    )
  ) + 
  scale_fill_manual(values = pie_cols) + 
  scale_y_discrete(limits = rev) + 
  expand_limits(y = c(6, 6)) + 
  ggridges::geom_density_ridges(
    alpha = 0.5,
    scale = 0.95
    ) + 
  facet_grid(~line) + 
  labs(x = "Normalised Score") + 
  theme(
    aspect.ratio = 3,
    axis.ticks = element_line(colour = "black"),
    axis.title.y= element_blank(),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey90"),
    panel.background = element_rect(fill = "white", colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black")
  )
  # ggh4x::force_panelsizes(
  #   rows = unit(8, "cm"),
  #   cols = unit(4, "cm")
  # )

  

# scater::plotReducedDim(
#   object = sce,
#   dimred = "X_scVI_MDE",
#   colour_by = "prog_norm"
# )
