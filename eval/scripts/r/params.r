# names
# sample_names <- list.dirs(
#   path = here("data", "raw", "splitpipe", "combined"),
#   recursive = FALSE
# ) |>
#   str_subset(pattern = "GL") |>
#   basename()
sample_names <- crossing(
  c("GL0028", "GL0038", "GL0095", "GL0128"),
  c("PDN", "PDO", "T")
) |> 
  unite(col = name) |> 
  pull(name)
sample_groups <- sample_names |>
  str_extract(
    pattern = "[^_]+"
  )
sample_list <- list(
  sample_names[1:3],
  sample_names[4:6],
  sample_names[7:9],
  sample_names[10:12]
) |>
  set_names(
    sample_groups[1],
    sample_groups[4],
    sample_groups[7],
    sample_groups[10],
  )

# sample colours
# line_cols <- c("#173f5f", "#3caea3", "#f6d55c", "#ed553b")
line_cols <-  c("#173f5f", "#ed553b", "#f6d55c", "#3caea3")
sample_cols <- map(
  .x = line_cols,
  .f = function(x) {
    tinter::tinter(
      x = x,
      steps = 2
    )
  }
) |>
  unlist() |>
  set_names(nm = sample_names)

# type colours
type_shapes <- c(16, 17, 15, 15) |> set_names(c("PDN", "PDO", "T", "TIS"))
ggplot2_colours <- function(n) {
  if (n %% 1 == 0 && n > 0) {
    hcl(h = seq(15, 375, length = n + 1), c = 100, l = 65)[1:n]
  } else {
    stop("err.")
  }
}
pie_cols <- ggplot2_colours(n = 9)[c(3, 6, 9)] |> 
  set_names("PDN", "PDO", "TIS")
pie_cols

# anno colours
source(here("eval", "scripts", "r", "annotation_colours.r"))
cluster_cols <- annotation_colours
names(cluster_cols) <- case_when(
  names(cluster_cols) == "AC-like" ~ "Astro",
  names(cluster_cols) == "MES-like" ~ "Mesenchymal",
  names(cluster_cols) == "OPC-like" ~ "Oligo",
  names(cluster_cols) == "NPC-like" ~ "Neuronal",
  names(cluster_cols) == "Unknown" ~ "Unassigned",
  .default = names(annotation_colours)
)
