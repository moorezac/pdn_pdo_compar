tibble(
  Sample = c("PDN", "PDN", "PDO", "PDO"),
  Outcome = factor(c("Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
  Percentage = c(100* 19/30, 100 * 11/30, 100 * 4/5, 100 * 1/5) 
) |> 
  mutate(Label = paste(round(Percentage, 2), "%")) |> 
  ggplot(
  mapping =
    aes(x = "", y = Percentage, fill = Outcome)
) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) + 
  facet_grid(~ Sample) + 
  geom_text(
    aes(
      label = Label
      ),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE
  ) + 
  guides(fill = guide_legend(title = "Success")) +
  scale_fill_manual(values = c("lightgreen", "palevioletred") |> set_names(c("Yes", "No"))) + 
  # facet_wrap(~ellipse) +
  theme(
    aspect.ratio = 1,
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.spacing.x = unit(0, "lines"),
    panel.spacing.y = unit(0, "lines"),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.text.y.left = element_text(angle = 0),
    strip.background = element_rect(fill = NA)
  ) -> plot_hit_rate


