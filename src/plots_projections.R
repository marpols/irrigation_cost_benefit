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

plot_projections2 <- function(dataset, variable, yrange,
                              title,
                              sub,
                              yaxis, 
                              txtsize = c("x.txt" = 10,
                                          "y.txt" = 15,
                                          "x.tit" = 15,
                                          "y.tit" = 15,
                                          "grids" = 15),
                              plotsize = c(22,18)){
  #plots projection data over time with se ribbons
  
  plot <- ggplot(dataset, aes(x = ian)) +
    geom_smooth(
      aes(y = .data[[variable]],
          group = ssp,
          fill = soil),
      colour = NA,    # ribbon only
      alpha = 0.15
    ) +
    geom_smooth(
      aes(y = .data[[variable]],
          group = ssp,
          colour = soil,
          linetype = ssp,
          alpha = ssp),
      se = FALSE,
      linewidth = 0.9
    ) +
    facet_grid(cols = vars(soil)) +
    coord_cartesian(ylim = yrange) +
    scale_fill_manual(values = c(ARY="#33A02C",
                                 CTW="#CAB2D6",
                                 CLO="#FDBF6F")) +
    scale_color_manual(values = c(ARY="#000000",
                                  CTW="#000000",
                                  CLO="#000000")) +
    scale_linetype_manual(values = c(ssp126="solid",
                                     ssp370="dashed", 
                                     ssp585="dotted"),
                          labels = c("SSP1-2.6",
                                     "SSP3-7.0",
                                     "SSP5-8.5")) +
    scale_alpha_manual(values = c(ssp126=1.00,
                                  ssp370=0.75,
                                  ssp585=0.55)) +
    guides(alpha = "none",
           fill = "none",
           color = "none",
           linetype = guide_legend(
             override.aes = list(
               fill = NA,  # choose ONE fill colour
               colour = "#000000"
             ))) +
    labs(title = title,
         subtitle = sub,
         x = "Year",
         y = yaxis,
         linetype = "SSP") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = txtsize[1],
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = txtsize[2]),
          axis.title.x = element_text(size = txtsize[3]),
          axis.title.y = element_text(size = txtsize[4]),
          strip.text.x = element_text(size = txtsize[5]),
          legend.text=element_text(size=15),
          legend.position = "bottom",)
  
  ggsave(sprintf("projections_%s_%s.png", title, sub),
         plot = plot,
         path = outdir,
         width = plotsize[1],
         height = plotsize[2],
         units = "cm")
  
  plot
}


plot_projections3 <- function(dataset, variable, yrange){
  
  plot <- ggplot(dataset, aes(x=ian)) +
    geom_smooth(aes(y = .data[[variable]],
                    group = ssp,
                    fill = ssp,
                    color = ssp)) +
    facet_grid(cols = vars(soil)) +
    coord_cartesian(ylim = yrange) +
    theme_minimal()
  plot
}


#payback periods
payback.period.proj <- function(dataset, mrkt_yld, 
                                soil_col,
                                ssp_col, 
                                mrkt_yld_col,
                                irr_col,
                                pb_col,
                                txtsize = c("x.txt" = 15,
                                            "y.txt" = 15,
                                            "x.tit" = 15,
                                            "y.tit" = 15,
                                            "grids" = 15),
                                plotsize = c(23,18)){
  
  dataset <- dataset |>
    dplyr::filter(mrktyld == mrkt_yld)
  
  dataset$irr.type <- factor(dataset$irr.type,
                             levels = c("pivot I",
                                        "pivot II",
                                        "hose reel + sprinkler",
                                        "hose reel + boom cart")
    )
      
  plot <- ggplot(dataset, aes(x = ssp, fill = soil)) +
    geom_col(aes(y = as.numeric(avg_payback_med),
                 alpha = "Mid."),
             position = "dodge") +
    geom_col(aes(y = as.numeric(years),
                 alpha = "None"),
             position = "dodge") +
    geom_hline(yintercept = 20,
               linewidth = 0.5,
               colour = "red") + 
    scale_alpha_manual(
      values = c(None = 1, Mid. = 0.6)
    ) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    coord_cartesian(ylim = c(0, 5)) +
    labs(title = "Payback Period",
         subtitle = sprintf("Marketable Yield %s%% of Total Yield",mrkt_yld),
         y = "Years",
         x = "None",
         fill = "Soil",
         alpha = "Environmental Costs") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = txtsize[1]),
          axis.text.y = element_text(size = txtsize[2]),
          # axis.title.x = element_text(size = txtsize[3]),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = txtsize[4]),
          strip.text.x = element_text(size = txtsize[5]),
          legend.text=element_text(size=15)) +
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
  
  ggsave(sprintf("payback_period_%d_proj.png", mrkt_yld),
         plot = plot,
         path = outdir,
         width = plotsize[1],
         height = plotsize[2],
         units = "cm")
  
}
