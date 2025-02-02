library(here)
library(tidyverse)
dat <- read_csv("/stornext/Bioinf/data/lab_brain_cancer/users/z_moore/manuscript/growth.csv")

dat |>
  mutate(Sample = type, Line = line) |>
  ggplot(
    mapping = aes(
      x = days,
      y = passage,
      colour = Line,
      shape = Sample,
      fill = Line
    )
  ) +
  geom_point() +
  scale_shape_manual(values = params$type_shapes) +
  geom_point(size = 3, key_glyph = "rect") +
  geom_line() +
  scale_colour_manual(values = params$line_colours) +
  scale_fill_manual(values = params$line_colours) +
  guides(
    colour = guide_legend(title = "Line"),
    shape = guide_legend(title = "Sample", override.aes = list(size = 3))
  ) +
  labs(
    x = "Days (Cumulative)",
    y = "Passage"
  ) +
  theme(
    # aspect.ratio = 1,
    panel.background = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_rect(fill = NA, colour = "black", linewidth = 0.25),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    text = element_text(size = 10),
    legend.spacing = unit(-0.1, "cm")
    # ) +
    # ggh4x::force_panelsizes(
    #   rows = unit(5, "cm"),
    #   cols = unit(10, "cm")
  ) -> plot_growth


dat |>
  mutate(Sample = type, Line = line) |>
  ggplot(
    mapping = aes(
      x = days,
      y = passage,
      colour = Line,
      shape = Sample,
      fill = Line
    )
  ) +
  geom_point() +
  scale_shape_manual(values = params$type_shapes) +
  geom_point(size = 3, key_glyph = "rect") +
  geom_line() +
  scale_colour_manual(values = params$line_colours) +
  scale_fill_manual(values = params$line_colours) +
  guides(
    colour = guide_legend(title = "Line"),
    shape = guide_legend(title = "Sample", override.aes = list(size = 3))
  ) +
  labs(
    x = "Days (Cumulative)",
    y = "Passage"
  ) +
  theme(
    # aspect.ratio = 1,
    panel.background = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_rect(fill = NA, colour = "black", linewidth = 0.25),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    text = element_text(size = 10),
    legend.spacing = unit(-0.1, "cm")
    # ) +
    # ggh4x::force_panelsizes(
    #   rows = unit(5, "cm"),
    #   cols = unit(10, "cm")
  )

half_dat <- dat |>
  arrange(line, type, passage) |> 
  mutate(a = lead(passage), b = lead(days)) |> 
  mutate(c = (a - passage)/2 + passage, d = (b - days)/2 + days) |> 
  mutate(e = b + (b - d), a = a + 0.5) |> 
  select(a, line, type, e) |> 
  rename(passage = a, days = e) |> 
  filter(passage == 5.5) |> 
  mutate(Sample = type, Line = line)

dat |> 
  bind_rows(
    half_dat
  )

dat <- dat |>
  mutate(
    label = case_when(
      line == "GL0028" & type == "PDN" & passage == 4 ~ "time of interrogation",
      line == "GL0038" & type == "PDN" & passage == 4 ~ "time of interrogation",
      line == "GL0095" & type == "PDN" & passage == 5 ~ "time of interrogation",
      line == "GL0128" & type == "PDN" & passage == 2 ~ "time of interrogation",
      line == "GL0028" & type == "PDO" & passage == 2 ~ "time of interrogation",
      line == "GL0038" & type == "PDO" & passage == 2 ~ "time of interrogation",
      line == "GL0095" & type == "PDO" & passage == 2 ~ "time of interrogation",
      line == "GL0128" & type == "PDO" & passage == 2 ~ "time of interrogation",
      .default = NA,
    )
  ) |>
  mutate(Sample = type, Line = line)
  
dat |>
  bind_rows(half_dat) |> 
  ggplot(
    mapping = aes(
      x = days,
      y = passage,
      colour = Line,
      shape = Sample,
      fill = Line,
    )
  ) +
  geom_point() +
  scale_shape_manual(values = params$type_shapes) +
  geom_point(size = 3, key_glyph = "rect") +
  geom_line() +
  scale_colour_manual(values = params$line_colours) +
  scale_fill_manual(values = params$line_colours) +
  geom_point(
    inherit.aes = FALSE,
    data = dat |> 
      filter(!is.na(label)),
    mapping = aes(x = days, y = passage),
    shape = 1,
    size = 8
  ) + 
  # ggrepel::geom_label_repel(
  #   data = bind_rows(dat, half_dat)|>
  #     mutate(
  #       label = case_when(
  #         line == "GL0028" & type == "PDN" & passage == 4 ~ "GL0028 PDN: time of interrogation",
  #         line == "GL0038" & type == "PDN" & passage == 4 ~ "GL0038 PDN: time of interrogation",
  #         line == "GL0095" & type == "PDN" & passage == 5 ~ "GL0095 PDN: time of interrogation",
  #         line == "GL0128" & type == "PDN" & passage == 2 ~ "GL0128 PDN: time of interrogation",
  #         line == "GL0028" & type == "PDO" & passage == 2 ~ "GL0028 PDO: time of interrogation,\nsample depleted",
  #         line == "GL0038" & type == "PDO" & passage == 2 ~ "GL0038 PDO: time of interrogation,\nsample depleted",
  #         line == "GL0095" & type == "PDO" & passage == 2 ~ "GL0095 PDO: time of interrogation,\nsample depleted",
  #         line == "GL0128" & type == "PDO" & passage == 2 ~ "GL0128 PDO: time of interrogation,\nsample depleted",
  #         .default = "",
  #       )
  #     ) |>
  #     mutate(Sample = type, Line = line),
  #   fill = "white",
  #   # colour = "black",
  #   angle = 0, 
  #   # vjust = 1, 
  #   force = 10, 
  #   force_pull = 0.1, 
  #   ylim = c(2.5, 5),
  #   size = 6/.pt, 
  #   # point.padding = 5,
  #   segment.color = "black",
  #   segment.linetype = 2, 
  #   show.legend = FALSE,
  #   alpha = 0.5
  #   ) + 
  coord_cartesian(ylim = c(0, 5)) + 
  guides(
    colour = guide_legend(title = "Line"),
    shape = guide_legend(title = "Sample", override.aes = list(size = 3))
  ) +
  labs(
    x = "Days (Cumulative)",
    y = "Passage"
  ) +
  theme(
    # aspect.ratio = 1,
    panel.background = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_rect(fill = NA, colour = "black", linewidth = 0.25),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    text = element_text(size = 10),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing = unit(-0.1, "cm")
    # ) +
    # ggh4x::force_panelsizes(
    #   rows = unit(5, "cm"),
    #   cols = unit(10, "cm")
  ) -> plot_growth

plot_growth

ggsave(
  filename = here("output/growth/plots/growth.pdf"),
  width = 10,
  height = 5,
  units = "cm"
)
EnvironmentModules::module_load("texlive")
system2(
  command = "pdfcrop",
  args = c(
    here("output/growth/plots/growth.pdf"),
    here("output/growth/plots/growth.pdf")
  ),
  stdout = NULL
)
# a <- plot_hit_rate + ggh4x::force_panelsizes(rows = unit(5, "cm"), cols = unit(5, "cm"))
# b <- plot_growth + ggh4x::force_panelsizes(rows = unit(7.5, "cm"), cols = unit(10, "cm"))
# a$facet$params$force.respect <- TRUE
# b$facet$params$force.respect <- TRUE
# patchwork::wrap_plots(
#  a,
#   b,
#   ncol = 1
#   )
# ggsave(
#   "btrg/collated_growth.pdf",
#   device = cairo_pdf,
#   width = 15,
#   height = 15,
#   units = "cm"
# )
#

dat |> 
  filter(
    line == "GL0028" & type == "PDN" & passage == 1 |
    line == "GL0038" & type == "PDN" & passage == 2 |
    line == "GL0095" & type == "PDN" & passage == 1 |
    line == "GL0128" & type == "PDN" & passage == 2
  ) |> 
  pull(days) |> mean()
