
pbpextplot_hist <- function(dt, dtext, pbcol) {
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
  
  
  pbp_all <- left_join(dt, dtext,
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
                  soilcode = factor(soilcode,
                                    levels = c("CLO", "CTW", "ARY")),
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
  
  pbp_long <- pbp_long |>
    dplyr::mutate(
      colour_group = dplyr::if_else(
        cost_scenario == "Without externalities",
        as.character(soilcode),
        as.character(cost_scenario)
      )
    )
  
  
  soils <- unique(as.character(pbp_long$soilcode))
  
  colour_values <- c(
    ARY="#33A02C",
    CTW="#CAB2D6",
    CLO="#FDBF6F"
  )
  
  shape_values <- setNames(rep(1, length(unique(
    pbp_long$cost_scenario
  ))), unique(pbp_long$cost_scenario))
  shape_values["Without externalities"] <- 15
  shape_values["Low"] <- 1
  shape_values["Mid"] <- 2
  shape_values["High"] <- 3
  
  size_values <- setNames(rep(3, length(unique(
    pbp_long$cost_scenario
  ))), unique(pbp_long$cost_scenario))
  size_values["Without externalities"] <- 10
  size_values["Low"] <- 8
  size_values["Mid"] <- 8
  size_values["High"] <- 8
  
  ggplot(
    pbp_long,
    aes(
      y = soilcode,
      x = payback,
      colour = colour_group,
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
    geom_vline(xintercept = 20, linetype = "dashed") +
    geom_point() +
    scale_shape_manual(values = shape_values) +
    scale_size_manual(values = size_values) +
    scale_colour_manual(
      values = colour_values,
      breaks = c(soils, "Low", "Mid", "High")
    ) +
    facet_grid(system ~ stncode, switch = "y") +
    labs(
      x = "Payback period (years)",
      y = "soil type",
      shape = "Cost scenario",
      size = "Cost scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.placement = "outside") +
    guides(colour = "none")
}


