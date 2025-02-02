plot_umap_coldata <- 
  function(obj, dimred = "UMAP", coldata, sample_name, shape = FALSE, shuffle = TRUE, width, height, units, colours) {
  
  dat <- reducedDim(obj, dimred) %>%
    as_tibble(
      .name_repair = ~ vctrs::vec_as_names(
        ..., 
        repair = "unique", 
        quiet = TRUE)
      ) %>% 
    # as_tibble(.name_repair = "unique", quiet = TRUE) |> 
    dplyr::select(1, 2)
  fill <- colData(obj) %>%
    as_tibble() %>%
    pull(coldata)
  dat <- cbind(
    dat,
    fill
  ) %>%
    # dplyr::rename(
    #   !!sym(paste0(dimred, 1)) := 1,
    #   !!sym(paste0(dimred, 2)) := 2
    # ) %>%
    as_tibble()
    # mutate(value = as.factor(value))
  
  # keyheight = height of individual boxes, not overall height
  key_height <- height / length(unique(dat$fill))
  
  if (missing(colours)) {
    set.seed(42)
    colours <- Polychrome::createPalette(
      N = dat$fill |>
        unique() |>
        length(),
      seedcolors = c(
        "#FF0000",
        "#00FF00",
        "#0000FF"
      ),
      range = c(
        30,
        70
      ),
      M = 10000
    )
    names(colours) <- NULL
  }
  
  if(shuffle) {
    dat <- dat |> 
      slice(sample(1:n()))
  }
    
  
  if(shape) {
    p <- ggplot(
      data = dat,
      aes(
        x = !!sym(colnames(dat)[1]),
        y = !!sym(colnames(dat)[2]),
        colour = !!sym(colnames(dat)[3]),
        fill = !!sym(colnames(dat)[3]),
        shape = !!sym(colnames(dat)[3])
      )
    )
  } else {
    p <- ggplot(
      data = dat,
      aes(
        x = !!sym(colnames(dat)[1]),
        y = !!sym(colnames(dat)[2]),
        colour = !!sym(colnames(dat)[3]),
        fill = !!sym(colnames(dat)[3])
      )
    )
  }
  
  p <- p +
    geom_point(key_glyph = draw_key_rect) + 
    theme(aspect.ratio = 1) + 
    scale_colour_manual(
      values = colours,
      guide = guide_legend(
        keyheight = grid::unit(
          x = key_height,
          units = units
        ),
        keywidth = grid::unit(
          x = 0.25,
          units = units
        ),
        title.position = "right",
        ncol = 1
      ),
      drop = FALSE
    ) + 
    scale_fill_manual(
      values = colours,
      drop = FALSE
    ) +
    theme(legend.title = element_blank()) +
    theme(
      panel.background = element_rect(
        fill = "transparent",
        colour = "black",
        linewidth = 0.05
      ),
      axis.ticks = element_line(
        colour = "black",
        linewidth = 0.05
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.box.spacing = unit(1, "pt"),
      plot.title = ggtext::element_markdown(),
      legend.key = element_rect(
        fill = "transparent",
        colour = "black",
        linewidth = 0.05
      )
    ) + 
    ggtitle(
      label = paste0(
        sample_name,
        "<br>",
        coldata
      )
    )
    
  
  # ensure consistent size
  p +  ggh4x::force_panelsizes(
    rows = grid::unit(x = width, units = units), 
    cols = grid::unit(x = height, units = units)
  )
  
}
