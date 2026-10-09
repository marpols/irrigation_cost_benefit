
pbpextplot_hist <- function(pbcol) {
  pbcols <- outer(sprintf("%s_ext", pbcol), c("min", "mid", "max"), paste, sep =
                    "_") |>
    as.vector()
  
  scenario_labels <- setNames(
    c(
      "Without externalities",
      "Low",
      "Mid",
      "High"
    ),
    c(pbcol, pbcols)
  )
  
  pbp_all <- left_join(PP_hist, PP_hist_ext,
                       by = c("soilcode", "stncode", "system"))
  pbp_long <- data.table::melt(
    pbp_all,
    id.vars = c("stncode", "soilcode", "system"),
    measure.vars = c(pbcol, pbcols),
    variable.name = "cost_scenario",
    value.name = "payback"
  ) |>
    dplyr::mutate(stncode = factor(stncode,
                                   levels = c("S", "NG", "HCC", "EP")),
                  system = factor(
                    system,
                    levels = c(
                      "pivot I",
                      "pivot II",
                      "hose reel + sprinkler",
                      "hose reel + boom cart"
                    )
                  ),
                  cost_scenario = factor(
                    dplyr::recode(
                    as.character(cost_scenario),
                    !!!scenario_labels
                  ),
                  levels = c(
                    c(
                      "Without externalities",
                      "Low",
                      "Mid",
                      "High"
                    )
                  ))
    )
  
  shape_values <- setNames(rep(1, length(unique(
    pbp_long$cost_scenario
  ))), unique(pbp_long$cost_scenario))
  shape_values["Without externalities"] <- 15
  
  size_values <- setNames(rep(3, length(unique(
    pbp_long$cost_scenario
  ))), unique(pbp_long$cost_scenario))
  size_values["Without externalities"] <- 4
  
  ggplot(
    pbp_long,
    aes(
      y = soilcode,
      x = payback,
      colour = cost_scenario,
      shape = cost_scenario,
      size = cost_scenario
    )
  ) +
    geom_line(
      aes(group = interaction(stncode, soilcode)),
      colour = "grey70",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    geom_point() +
    scale_shape_manual(values = shape_values) +
    scale_size_manual(values = size_values) +
    facet_grid(system ~ stncode) +
    labs(
      x = "Payback period (years)",
      y = "soil type",
      colour = "Cost scenario",
      shape = "Cost scenario",
      size = "Cost scenario"
    ) +
    theme_minimal()
}

pbpextplot_hist("PP90") + labs(subtitle = "Marketable Yield 90% of Total Yield")
pbpextplot_hist("PP70") + coord_cartesian(xlim = c(0,125)) + 
  labs(subtitle = "Marketable Yield 70% of Total Yield")
