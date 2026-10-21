plot_points_grid <- function(dataset,
                             variable,
                             x_axis = "stncode",
                             factors = character(),
                             title = character(),
                             grid_cols = "soilcode",
                             grid_rows = "ssp",
                             axis_title_x = x_axis,
                             axis_title_y = variable,
                             colour = "red",
                             shape_by = "soilcode",
                             method = "lm") {
  if (!rlang::is_empty(factors)) {
    dataset[[x_axis]]  <- factor(dataset[[x_axis]] , levels = factors)
  }
  
  cols_expr <- if (!is.null(grid_cols))
    ggplot2::vars(!!dplyr::sym(grid_cols))
  else
    NULL
  rows_expr <- if (!is.null(grid_rows))
    ggplot2::vars(!!dplyr::sym(grid_rows))
  else
    NULL
  
  point_layer <- if (colour %in% names(dataset)) {
    ggplot2::geom_point(ggplot2::aes(colour = .data[[colour]], shape = .data[[shape_by]]))
  } else {
    ggplot2::geom_point(ggplot2::aes(colour = colour, .data[[shape_by]]))
  }
  
  ggplot2::ggplot(dataset,
                  ggplot2::aes(x = .data[[x_axis]], y = .data[[variable]])) +
    point_layer +
    ggplot2::facet_grid(cols = cols_expr, rows = rows_expr, switch = "y") +
    ggplot2::geom_smooth(
      aes(
        group = .data[[colour]],
        colour = "#000000",
        linetype = .data[[colour]]
      ),
      method = method,
      se = FALSE,
      linewidth = 0.5
    ) +
    ggplot2::labs(title = title, x = axis_title_x, y = axis_title_y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.placement = "outside")
  
}



