plot_umap_gene <-
  function(obj, dimred = "UMAP", gene_name, sample_name, width, height, units) {
    if (missing(sample_name)) {
      sample_name <- NULL
    }

    gene_id <- rowData(obj) |>
      as_tibble() |>
      filter(gene_name == !!gene_name) |>
      pull(gene_id)

    # if (is_empty(gene_id)) {
    #   message("no gene_id found")
    #   break
    # }

    dat <- reducedDim(obj, dimred) %>%
      as_tibble(
        .name_repair = ~ vctrs::vec_as_names(
          ..., 
          repair = "unique", 
          quiet = TRUE)
      ) %>% 
      # as_tibble(.name_repair = "minimal") |> 
      select(1, 2)

    if (is_empty(gene_id)) {
      fill <- rep_len(0, length.out = nrow(dat))
    } else {
      fill <- logcounts(obj)[gene_id, ] %>%
        as_tibble(.name_repair = "minimal")
    }

    dat <- cbind(
      dat,
      fill
    ) %>%
      dplyr::rename(
        UMAP1 = 1,
        UMAP2 = 2,
        value = 3
      ) %>%
      as_tibble()

    p <- ggplot(
      data = dat,
      aes(
        x = UMAP1,
        y = UMAP2,
        colour = value
      )
    ) +
      geom_point() +
      theme(aspect.ratio = 1) +
      viridis::scale_color_viridis(
        guide = guide_colorbar(
          barheight = grid::unit(
            x = height,
            units = units
          ),
          barwidth = grid::unit(
            x = 0.25,
            units = units
          ),
          title.position = "right",
          frame.colour = "black",
          frame.linewidth = 0.05
        )
      ) +
      theme(
        legend.title = element_blank(),
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
        plot.title = ggtext::element_markdown(),
        legend.box.spacing = unit(1, "pt")
      ) +
      ggtitle(
        label = paste0(
          sample_name,
          "<br>",
          "*", gene_name, "*"
        )
      )

    p + ggh4x::force_panelsizes(
      rows = grid::unit(x = width, units = units),
      cols = grid::unit(x = height, units = units)
    )
  }
