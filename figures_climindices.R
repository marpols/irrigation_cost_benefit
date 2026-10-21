
lapply(list("drydays", "TX30"),\(v){
  plot_points_grid(
  climind_hist,
  var = v,
  x_axis = "ian",
  grid_cols = "stncode",
  grid_rows = NULL,
  colour = "stncode",
  shape_by = "stncode"
) +
  ggplot2::scale_colour_manual(values = stn_colours,
  name = "Station") +
  labs(linetype = "",
       shape = "Station",
       title = "") +  guides(linetype = guide_legend(override.aes = list(colour = "black")))
})

lapply(list("drydays", "TX30"),\(v){
  plot_points_grid(
    climind_proj,
    var = v,
    x_axis = "ian",
    grid_cols = "model",
    grid_rows = "ssp",
    colour = "ssp",
    shape_by = "ssp"
  ) +
    ggplot2::scale_colour_manual(values = ssp_colours,
                                 name = "ssp") +
    labs(linetype = "",
         shape = "ssp",
         title = "") +  
    guides(linetype = guide_legend(override.aes = list(colour = "black")))
})

hist_SPEI <- hist_SPEI |>
  dplyr::mutate(spei_class = factor(spei_class, levels = classes),
                stncode = factor(stncode, levels = stns))

plot_points_grid(
  hist_SPEI[MONTH == 6, ],
  var = "spei",
  x_axis = "YEAR",
  grid_cols = "stncode",
  grid_rows = NULL,
  colour = "stncode",
  shape_by = "stncode"
) +
  ggplot2::scale_colour_manual(values = stn_colours,
                               name = "Station") +
  labs(linetype = "",
       shape = "Station",
       title = "") +  
  guides(linetype = guide_legend(override.aes = list(colour = "black"))) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

plot_points_grid(
  proj_SPEI[MONTH == 6, ],
  var = "spei",
  x_axis = "YEAR",
  grid_cols = "model",
  grid_rows = "ssp",
  colour = "ssp",
  shape_by = "ssp"
) +
  ggplot2::scale_colour_manual(values = ssp_colours,
                               name = "ssp") +
  labs(linetype = "",
       shape = "ssp",
       title = "") +  
  guides(linetype = guide_legend(override.aes = list(colour = "black"))) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

