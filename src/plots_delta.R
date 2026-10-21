plots_delta_hist <- function(df, var){
  
  dataset <- df |>
    dplyr::mutate(
      stncode = factor(stncode,
                       levels = c("S", "NG", "HCC", "EP"))
    )
  
  STICSdt::plot_variable_grid(dataset, var, x_axis = "ian",
                              by = "soilcode",
                              grid_cols = "stncode",
                              grid_rows = "soilcode") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(
        x = .data[["ian"]],
        y = .data[[var]]
      ),
      method = "lm",
      se = FALSE,
      linewidth = 0.5
    )
    
}

plots_delta_proj <- function(df, var){
  
  dataset <- df |>
  
    STICSdt::get_mean(var, by = c("ssp", "soilcode","ian"))  |>
    dplyr::mutate(
      ssp = factor(ssp,
                   levels = c("ssp126", "ssp370", "ssp585"))
    ) 
  
  STICSdt::plot_variable_grid(dataset, paste0(var, "_mean"), x_axis = "ian",
                              by = "soilcode",
                              grid_cols = "ssp",
                              grid_rows = "soilcode") + 
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(
        x = .data[["ian"]],
        y = .data[[paste0(var, "_mean")]]
      ),
      method = "lm",
      se = FALSE,
      linewidth = 0.5
    ) + 
    ggplot2::geom_hline(yintercept = 0) 
  
}


