library(tidyverse)

incu_dat <- read_csv(
  file = "/stornext/Bioinf/data/lab_brain_cancer/users/z_moore/manuscript/area_pdo.csv"
)

incu_plot_38 <- incu_dat |>
  mutate(
    Sample = str_split_i(sample, "_", 1) |> toupper(),
    Treatment = str_split_i(sample, "_", 2) |> toupper()
  ) |>
  mutate(Treatment = case_when(
    Treatment == "TMZ" & Sample == "PDO" ~ "50\u03bcM TMZ",
    Treatment == "TMZ" & Sample == "PDN" ~ "10\u03bcM TMZ",
    Treatment == "VEH" ~ "Vehicle"
  )) |>
  mutate(area = as.numeric(area)) |>
  ggplot(
    mapping = aes(
      x = time,
      y = area,
      shape = Sample,
      colour = Treatment
    )
  ) +
  geom_point(size = 3) +
  stat_smooth(se = FALSE) +
  labs(
    y = "Normalised Area",
    x = "Time (hrs.)"
  ) +
  scale_colour_manual(
    values = c("darkorange", "orange", line_cols[2]) |>
      set_names(c("50\u03bcM TMZ", "10\u03bcM TMZ", "Vehicle"))
  ) +
  theme(
    # aspect.ratio = 1/2,
    axis.ticks = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey90"),
    panel.background = element_rect(fill = "white", colour = "black")
  ) +
  ggtitle("GL0038")

ctg_dat <- read_csv(
  file = "/stornext/Bioinf/data/lab_brain_cancer/users/z_moore/manuscript/ctg_incu.csv"
)

incu_ctg_plot <- ctg_dat |>
  filter(Line == "GL0038") |>
  group_by(Sample, Line) |>
  mutate(sd = sd(value), mean = mean(value)) |>
  ungroup() |>
  ggplot(
    mapping = aes(
      x = Line,
      y = mean,
      fill = Sample
    )
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    mapping = aes(
      ymin = mean - sd,
      ymax = mean + sd
    ),
    position = position_dodge()
  ) +
  scale_fill_manual(values = pie_cols) +
  labs(y = "CTG (% of Vehicle)") +
  theme(
    axis.ticks = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey90"),
    panel.background = element_rect(fill = "white", colour = "black")
  )

library(png)
img <- readPNG("/stornext/General/scratch/GP_Transfer/Zachery Moore/test.png")

a <- ggplot() +
  annotation_custom(
    grid::rasterGrob(img,
      width = ggplot2::unit(1, "npc"),
      height = ggplot2::unit(1, "npc")
    ),
    -Inf, Inf, -Inf, Inf
  ) +
  theme(panel.background = element_blank())

patchwork::wrap_plots(
  a + theme(aspect.ratio = 667.1 / 1288.1),
  incu_plot_38 + theme(aspect.ratio = 1),
  incu_ctg_plot + theme(aspect.ratio = 1)
)
