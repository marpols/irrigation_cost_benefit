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

plots_delta_hist(analysis_hist, "dnl") + labs(y = expression(paste(Delta, "Qles (kg N ", ha^-1, " )")),
                                              x = "year",
                                              title = "Change in N leaching") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  theme(panel.spacing.x = unit(0.5, "cm"))

plots_delta_hist(analysis_hist, "dn2o")
plots_delta_hist(analysis_hist[dplntN !=0,], "dplntN")
plots_delta_hist(analysis_hist, "totir")

plots_delta_proj(analysis_proj[dnl != 0,], "dnl") +
  coord_cartesian(ylim = c(-20, 15)) + labs(y = expression(paste(Delta, "Qles (kg N ", ha^-1, " )")),
                                            x = "year",
                                            title = "Change in N leaching") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  theme(panel.spacing.x = unit(0.5, "cm"))

plots_delta_proj(analysis_proj, "dn2o")
plots_delta_proj(analysis_proj[dplntN !=0,], "dplntN")
plots_delta_proj(analysis_proj, "totir")
