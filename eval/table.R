library(mmtable2)
library(gt)
clin_table <- read_csv(
  file = "/stornext/Bioinf/data/lab_brain_cancer/users/z_moore/table.csv"
)

clin_table |> 
  gt() |> 
  tab_header(
    title = "Clinical Information"
  )
  pivot_longer(
    cols = 2:5,
    names_to = "line"
  ) |> 
  mutate(row = ID) |>
  gt()
  header_top(line) + 
  header_left(row)

plot(table_plot) + 
  ggh4x::force_panelsizes(
    rows = unit(5, "cm")
  )
