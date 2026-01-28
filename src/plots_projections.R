plot_projections <- function(dataset, variable){
  #average over all soil types
  plot <- ggplot(dataset, aes(x = ian)) +
    geom_line(aes(y = .data[[variable]],
                  color = model)) +
    facet_grid(cols = vars(soil),
               rows = vars(ssp)) +
    theme_minimal()
  plot
}

plot_projections2 <- function(dataset, variable, yrange){
  # averages <- dataset |>
  #   group_by(ssp, ian, soil) |>
  #   summarise(average = mean(.data[[variable]]),
  #             min = min(.data[[variable]]),
  #             max = max(.data[[variable]]))
  
  plot <- ggplot(dataset, aes(x=ian)) +
    geom_smooth(aes(y = .data[[variable]],
                    group = ssp,
                    fill = ssp,
                    color = ssp)) +
    facet_grid(cols = vars(soil)) +
    coord_cartesian(ylim = yrange) +
    theme_minimal()
  plot
  
  # plot2 <- ggplot(averages, aes(x=ian)) +
  #   geom_line(aes(y = average,
  #                 group = ssp)) +
  #   geom_ribbon(aes(ymax = max,
  #                   ymin = min,
  #                   group = ssp,
  #                   fill = ssp)) + 
  #   facet_grid(cols = vars(soil)) +
  #   theme_minimal()
  # plot2
}

#payback periods
payback.period.proj <- function(dataset, mrkt_yld, 
                                 soil_col,
                                 ssp_col, 
                                 mrkt_yld_col,
                                 irr_col,
                                 pb_col){
  
  dataset <- dataset |>
    dplyr::filter(mrktyld == mrkt_yld)
  
  dataset$irr.type <- factor(dataset$irr.type,
                             levels = c("pivot I",
                                        "pivot II",
                                        "hose reel + sprinkler",
                                        "hose reel + boom cart")
    )
      
  plot <- ggplot(dataset, aes(x = ssp, fill = soil)) +
    geom_col(aes(y = as.numeric(avg_payback_max),
                 alpha = "High"),
             position = "dodge") +
    geom_col(aes(y = as.numeric(years),
                 alpha = "None"),
             position = "dodge") +
    geom_hline(yintercept = 20,
               linewidth = 0.5,
               colour = "red") + 
    scale_alpha_manual(
      values = c(None = 1, High = 0.6)
    ) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    coord_cartesian(ylim = c(0, 5)) +
    labs(title = "Payback Period",
         subtitle = sprintf("Marketable Yield %s%% of Total Yield",mrkt_yld),
         y = "Years",
         x = "SSP",
         fill = "Soil",
         alpha = "Environmental Costs") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 14)) +
    facet_grid(cols = vars(irr.type)) +
    guides(
      alpha = guide_legend(
        override.aes = list(
          fill = "#CAB2D6",  # choose ONE fill colour
          colour = NA
        )
        )
      ) +
    scale_x_discrete(
      labels = function(x) recode(
        x,
        ssp126  = "SSP1-2.6",
        ssp370 = "SSP3-7.0",
        ssp585  = "SSP5-8.5"
      )
    )
  plot
  
  ggsave(sprintf("payback_period_%d.png", mrkt_yld),
         plot = plot,
         path = outdir,
         width = 23,
         height = 18,
         units = "cm")
  
  
  
}
