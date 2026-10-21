
# Historical --------------------------------------------------------------

stnfac <- c("S", "NG", "HCC", "EP")
lapply(list(climhist_summary, climhist_summarygs), \(dt) {
  lapply(
    list(
      "Precipitation_cum_avg",
      "GDD_cum_avg",
      "WindSpeed_avg",
      "SolarRad_avg"
    ),
    \(v) {
      STICSdt::plot_boxplot(dt, factors = stnfac, v, colour = "lightblue")
    }
  )
})

# Projections -------------------------------------------------------------


sspfac <- c("ssp126", "ssp370", "ssp585")
lapply(list(climproj_summary, climproj_summarygs), \(dt) {
  lapply(
    list(
      "Precipitation_cum_avg",
      "GDD_cum_avg",
      "WindSpeed_avg",
      "SolarRad_avg"
    ),
    \(v) {
      STICSdt::plot_boxplot_grid(
        dt,
        variable = v,
        x_axis = "model",
        grid_cols = "ssp",
        grid_rows = NULL
      ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  )
})



# REF ---------------------------------------------------------------------

lapply(list(climREF_sum, climREF_sumgs), \(dt) {
  lapply(
    list(
      "Precipitation_cum_avg",
      "GDD_cum_avg",
      "WindSpeed_avg",
      "SolarRad_avg"
    ),
    \(v) {
      STICSdt::plot_boxplot_grid(
        climREF_sum,
        variable = v,
        x_axis = "model",
        grid_cols = "ssp",
        grid_rows = NULL,
        colour = "yellow"
      ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  )
})
