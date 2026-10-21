classes <- c("Extremely Wet",
             "Very Wet",
             "Moderate Wet",
             "Normal",
             "Moderate Dry",
             "Very Dry",
             "Extremely Dry")


stns <- c("S", "NG", "HCC", "EP")
soils <- c("CLO", "CTW", "ARY")


plothist <- lapply(list("fixed", "dynamic"), \(exp){
  data_hist[[exp]]$analysis |>
    dplyr::mutate(spei_class = factor(spei_class, levels = classes),
                  stncode = factor(stncode, levels = stns),
                  soilcode = factor(soilcode, levels = soils))
}) |> purrr::set_names(c("fixed", "dynamic"))

plothist2 <- lapply(list("fixed", "dynamic"), \(exp){
 data_hist2[[exp]] |>
    dplyr::mutate(spei_class = factor(spei_class, levels = classes),
                  stncode = factor(stncode, levels = stns),
                  soilcode = factor(soilcode, levels = soils))
}) |> purrr::set_names(c("fixed", "dynamic"))

plotproj <- lapply(list("fixed", "dynamic"), \(exp){
  data_proj[[exp]]$analysis |>
    dplyr::mutate(spei_class = factor(spei_class, levels = classes),
                  soilcode = factor(soilcode, levels = soils))
}) |> purrr::set_names(c("fixed", "dynamic"))

plotREF <- data_REF |>
  dplyr::mutate(spei_class = factor(spei_class, levels = classes),
                soilcode = factor(soilcode, levels = soils))


# yield -------------------------------------------------------------------


lapply(list("fixed", "dynamic"), \(exp){
  STICSdt::plot_boxplot_grid(plothist[[exp]], "yield",
                             x_axis = "soilcode",
                             grid_cols = "stncode",
                             colour = "soilcode",
                             grid_rows = NULL) +
    ggplot2::coord_cartesian(ylim = c(0, 50)) + 
    ggplot2::scale_fill_manual(values = soil_colours,
                               name = "Soil Type") +
    labs(title = sprintf("Historical - %s planting date", exp))
})

lapply(list("fixed", "dynamic"), \(exp){
  STICSdt::plot_boxplot_grid(plotproj[[exp]], "yield",
                             x_axis = "soilcode",
                             grid_cols = "ssp",
                             colour = "soilcode",
                             grid_rows = NULL) +
    ggplot2::coord_cartesian(ylim = c(0, 55)) + 
    ggplot2::scale_fill_manual(values = soil_colours,
                               name = "Soil Type") +
    labs(title = sprintf("Projected - %s planting date", exp))
})


lapply(list(list("ssp", NULL), list("model", "ssp")), \(r){
  STICSdt::plot_boxplot_grid(plotREF, "pdsfruitfrais_max",
                             x_axis = "soilcode",
                             grid_cols = r[[1]],
                             colour = "soilcode",
                             grid_rows = r[[2]]) +
    ggplot2::scale_y_continuous(limits = c(0, 55), breaks = seq(0,55, by = 10)) + 
    ggplot2::scale_fill_manual(values = soil_colours,
                               name = "Soil Type") +
    ggplot2::labs(title = "REF")
})


STICSdt::plot_boxplot_grid(plothist$fixed, "yield",
                           x_axis = "soilcode",
                           grid_cols = NULL,
                           colour = "soilcode",
                           grid_rows = NULL) +
  ggplot2::scale_y_continuous(limits = c(0, 55), breaks = seq(0,55, by = 10)) + 
  ggplot2::scale_fill_manual(values = soil_colours,
                             name = "Soil Type") +
  ggplot2::labs(title = "Yield across all weather stations (2001-2024)")


# deltas ------------------------------------------------------------------

deltas_hist <- lapply(plothist, \(dt){
  vars <- list("dnl", "dn2o", "dplntN", "yg")
  lapply(vars, \(var){
    plot_points_grid(
      dt,
      var,
      x_axis = "totir",
      grid_cols = "stncode",
      grid_rows = NULL,
      colour = "soilcode"
    ) + ggplot2::scale_colour_manual(values = soil_colours,
                                     name = "Soil Type") +
      labs(
        linetype = "",
        shape = "Soil Type",
        title = ""
      ) +   guides(
        linetype = guide_legend(override.aes = list(colour = "black"))
      )
  }) |> purrr::set_names(vars)
}) |> purrr::set_names("fixed", "dynamic")



deltas_proj <- lapply(plotproj, \(dt){
  vars <- list("dnl", "dn2o", "dplntN", "yg")
  lapply(vars, \(var){
    plot_points_grid(
      dt,
      var,
      x_axis = "totir",
      grid_cols = "ssp",
      grid_rows = NULL,
      colour = "soilcode"
    ) + ggplot2::scale_colour_manual(values = soil_colours,
                                     name = "Soil Type") +
      labs(
        linetype = "",
        shape = "Soil Type",
        title = ""
      ) +   guides(
        linetype = guide_legend(override.aes = list(colour = "black"))
      )
  }) |> purrr::set_names(vars)
}) |> purrr::set_names("fixed", "dynamic")


# yg and stress -----------------------------------------------------------

lapply(list("fixed", "dynamic"), \(pds){
  lapply(list("swfac_noirr", "turfac_noirr",
              "swfac_min", "turfac_min",
              "swfac2moy_noirr", "swfac2moy_min",
              "turfac2moy_noirr", "turfac2moy_min"), \(v){
                plot_points_grid(plothist[[pds]],
                                 var = v,
                                 x_axis = "ian",
                                 grid_cols = "stncode",
                                 grid_rows = NULL,
                                 colour = "soilcode") +
                  ggplot2::scale_colour_manual(values = soil_colours,
                                               name = "Soil Type") +
                  labs(
                    linetype = "",
                    shape = "Soil Type",
                    title = ""
                  ) +   guides(
                    linetype = guide_legend(override.aes = list(colour = "black"))
                  )
              })
}) 

lapply(list("fixed", "dynamic"), \(pds){
  lapply(list("swfac_noirr", "turfac_noirr",
              "swfac_min", "turfac_min",
              "swfac2moy_noirr", "swfac2moy_min",
              "turfac2moy_noirr", "turfac2moy_min"), \(v){
                plot_points_grid(plotproj[[pds]],
                                 var = v,
                                 x_axis = "ian",
                                 grid_cols = "model",
                                 grid_rows = "ssp",
                                 colour = "soilcode") + 
                  ggplot2::scale_colour_manual(values = soil_colours,
                                               name = "Soil Type") +
                  labs(
                    linetype = "",
                    shape = "Soil Type",
                    title = ""
                  ) +   guides(
                    linetype = guide_legend(override.aes = list(colour = "black"))
                  )
              })
}) 





lapply(list("fixed", "dynamic"), \(pds){
  lapply(list("swfac_noirr", "turfac_noirr",
              "swfac_min", "turfac_min",
              "swfac2moy_noirr", "swfac2moy_min",
         "turfac2moy_noirr", "turfac2moy_min"), \(v){
           plot_points_grid(plothist[[pds]],
                            var = "yg",
                            x_axis = v,
                            grid_cols = "stncode",
                            grid_rows = NULL,
                            colour = "soilcode") +
             ggplot2::scale_colour_manual(values = soil_colours,
                                          name = "Soil Type") +
             labs(
               linetype = "",
               shape = "Soil Type",
               title = ""
             ) +   guides(
               linetype = guide_legend(override.aes = list(colour = "black"))
             )
         })
}) 

lapply(list("fixed", "dynamic"), \(pds){
  lapply(list("swfac2moy_noirr", "swfac2moy_min",
              "turfac2moy_noirr", "turfac2moy_min"), \(v){
                plot_points_grid(plotproj[[pds]],
                                 var = "yg",
                                 x_axis = v,
                                 grid_cols = "ssp",
                                 grid_rows = NULL,
                                 colour = "soilcode") + 
                  ggplot2::scale_colour_manual(values = soil_colours,
                                               name = "Soil Type") +
                  labs(
                    linetype = "",
                    shape = "Soil Type",
                    title = ""
                  ) +   guides(
                    linetype = guide_legend(override.aes = list(colour = "black"))
                  )
              })
}) 

# payback periods ---------------------------------------------------------

pbpplots <- mapply(\(r, func){
  lapply(list("fixed", "dynamic"), \(pds){
    lapply(list("PP70", "PP90"), \(my){
      func(r[[pds]]$pbps, "PP70") +
        labs(subtitle = sprintf("%s Planting Date, Marketable Yield %s%% of Total Yield",
                                stringr::str_to_title(pds),
                                stringr::str_remove(my,"PP")
                                ))
    }) |> purrr::set_names("70", "90")
  }) |> purrr::set_names("fixed", "dynamic")
}, list(results_hist, results_proj), 
list(pbpplot_hist, pbpplot_proj),
SIMPLIFY = FALSE) |> purrr::set_names("hist", "proj")

pbpplots_histext <- lapply(list("fixed", "dynamic"), \(pds){
  lapply(list("PP70", "PP90"), \(my){
    pbpextplot_hist(results_hist[[pds]]$pbps,
                    results_histext[[pds]]$pbps,
                    my) +
      coord_cartesian(xlim = c(0,100)) +
      labs(subtitle = sprintf("%s Planting Date, Marketable Yield %s%% of Total Yield",
                                                              stringr::str_to_title(pds),
                                                              stringr::str_remove(my,"PP")
      ))
  }) |> purrr::set_names("70", "90")
}) |> purrr::set_names("fixed", "dynamic")

pbpplots_projext <- lapply(list("fixed", "dynamic"), \(pds){
  lapply(list("PP70", "PP90"), \(my){
    pbpextplot_proj(results_proj[[pds]]$pbps_mean,
                    results_projext[[pds]]$pbps_mean,
                    my) +
      coord_cartesian(xlim = c(0,10)) +
      labs(subtitle = sprintf("%s Planting Date, Marketable Yield %s%% of Total Yield",
                              stringr::str_to_title(pds),
                              stringr::str_remove(my,"PP")
      ))
  }) |> purrr::set_names("70", "90")
}) |> purrr::set_names("fixed", "dynamic")




