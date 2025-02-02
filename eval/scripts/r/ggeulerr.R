library(tidyverse)
library(eulerr)
library(ggforce)

#' Area proportional venn diagrams
#'
#' This functions uses eulerr::euler to plot area proportional venn diagramms
#' but plots it using ggplot2
#'
#' @param combinations set relationships as a named numeric vector, matrix, or data.frame(See `eulerr::euler`)
#' @param show_quantities whether to show number of intersecting elements
#' @param show_labels whether to show set names
#' @param ... further arguments passed to eulerr::euler
ggeulerr <- function(combinations, show_quantities = TRUE, show_labels = TRUE, ...) {
  data <-
    eulerr::euler(combinations = combinations) %>%
    plot(quantities = show_quantities) %>%
    pluck("data")
  
  tibble() %>%
    ggplot() +
    ggforce::geom_ellipse(
      data = data$ellipses %>% as_tibble(rownames = "Set"),
      mapping = aes(x0 = h, y0 = k, a = a, b = b, angle = 0, fill = Set),
      alpha = 0.5
    ) +
    geom_label(
      data = {
        data$centers %>%
          mutate(
            label = labels %>% map2(quantities, ~ {
              if (!is.na(.x) && !is.na(.y) && show_labels) {
                paste0(.x, "\n", sprintf(.y, fmt = "%.5g"))
              } else if (!is.na(.x) && show_labels) {
                .x
              } else if (!is.na(.y)) {
                .y
              } else {
                ""
              }
            })
          )
      },
      mapping = aes(x = x, y = y, label = label)
    ) +
    theme(panel.grid = element_blank()) +
    coord_fixed() +
    scale_fill_hue()
}

#
# Example
#

# list(
#   A = c("A", "B", "C"),
#   B = c("A", "D"),
#   C = "A",
#   D = c("A", "E")
# ) %>%
#   ggeulerr() +
#   labs(title = "Intersections of sets")
