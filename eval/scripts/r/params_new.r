params <- vector(mode = "list", length = 0)
params$sample_names = crossing(
    c("GL0028", "GL0038", "GL0095", "GL0128"),
    c("PDN", "PDO", "T")
  ) |> 
    unite(col = name) |> 
    pull(name)

params$sample_groups <- params$sample_names |>
  str_extract(
    pattern = "[^_]+"
  )
params$sample_list <- list(
  params$sample_names[1:3],
  params$sample_names[4:6],
  params$sample_names[7:9],
  params$sample_names[10:12]
) |>
  set_names(
    params$sample_groups[1],
    params$sample_groups[4],
    params$sample_groups[7],
    params$sample_groups[10],
  )

# sample colours
# line_cols <- c("#173f5f", "#3caea3", "#f6d55c", "#ed553b")
params$line_colours <-  c("#173f5f", "#ed553b", "#f6d55c", "#3caea3") |> 
  set_names("GL0028", "GL0038", "GL0095", "GL0128")
params$sample_colours <- map(
  .x = params$line_colours,
  .f = function(x) {
    tinter::tinter(
      x = x,
      steps = 2
    )
  }
) |>
  unlist() |>
  set_names(nm = params$sample_names)

# type colours
params$type_shapes <- c(16, 17, 15, 15) |> set_names(c("PDN", "PDO", "T", "TIS"))
ggplot2_colours <- function(n) {
  if (n %% 1 == 0 && n > 0) {
    hcl(h = seq(15, 375, length = n + 1), c = 100, l = 65)[1:n]
  } else {
    stop("err.")
  }
}
params$type_colours <- ggplot2_colours(n = 9)[c(3, 6, 9, 9)] |> 
  set_names("PDN", "PDO", "T", "TIS")
# pie_cols

params$annotation_colours <- source(here("eval/scripts/r/annotation_colours_new.r"))$value
# names(params$annotation_colours) <- case_when(
#   names(params$annotation_colours) == "AC-like" ~ "Astro",
#   names(params$annotation_colours) == "MES-like" ~ "Mesenchymal",
#   names(params$annotation_colours) == "OPC-like" ~ "Oligo",
#   names(params$annotation_colours) == "NPC-like" ~ "Neuronal",
#   names(params$annotation_colours) == "Unknown" ~ "Unassigned",
#   .default = names(params$annotation_colours)
# )