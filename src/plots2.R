payback.period.plot2 <- function(dataset, mrkt_yld, 
                                 soil_col,
                                 stn_col, 
                                 mrkt_yld_col,
                                 irr_col,
                                 pb_col){
  
  dataset <- dataset |>
    dplyr::filter(.data[[mrkt_yld_col]] == mrkt_yld) |>
    dplyr::mutate(
      stn_code = factor(.data[[stn_col]],
                        levels = c("S", "NG", "HCC", "EP")),
      irrigation_type = factor(.data[[irr_col]],
                               levels = c("pivot I",
                                          "pivot II",
                                          "hose reel + sprinkler",
                                          "hose reel + boom cart")
      )
    )
  
  plot <- ggplot(dataset, aes(stn_code)) +
    geom_col(aes(y = .data[[pb_col]],
                 fill = .data[[soil_col]]),
             position = "dodge") +
    geom_hline(yintercept = 20,
               linewidth = 0.5,
               colour = "red") +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    coord_cartesian(ylim = c(0, 60)) +
    labs(title = "Payback Period",
         subtitle = sprintf("Marketable Yield %s%% of Total Yield",mrkt_yld),
         y = "Years",
         x = "weather station",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_grid(cols = vars(irrigation_type))
  
  ggsave(sprintf("payback_period_%d.png", mrkt_yld),
         plot = plot,
         path = outdir,
         width = 20,
         height = 15,
         units = "cm")

  plot
  
}

payback.period.plot2(paybackperiods_avg_all2, 90, "soil", "stn", "mrktyld", "irr.type", "avg_payback")

library(dplyr)
library(tidyr)
library(ggplot2)

increase.plot <- function(dataset, mrkt_yld, 
                          stn_col,
                          mrkt_yld_col,
                          irr_col){
  thr <- 20
  
  df_delta <- dataset |>
    dplyr::filter(.data[[mrkt_yld_col]] == mrkt_yld) |>
    mutate(
      stn_code = factor(.data[[stn_col]], levels = c("S","NG","HCC","EP")),
      irrigation_type = factor(.data[[irr_col]],
                               levels = c("pivot I",
                                          "pivot II",
                                          "hose reel + sprinkler",
                                          "hose reel + boom cart")
      )
    ) |>
    transmute(
      stn_code, soil, irrigation_type,
      d_min = avg_payback_min - avg_payback,
      d_med = avg_payback_med - avg_payback,
      d_max = avg_payback_max - avg_payback
    ) |>
    pivot_longer(starts_with("d_"), names_to = "case", values_to = "delta_years") |>
    mutate(case = recode(case, d_min="Low", d_med="Mid", d_max="High"),
           case = factor(case, levels=c("Low", "Mid", "High")))
  
  ggplot(df_delta, aes(x = stn_code, y = delta_years, fill = soil)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    facet_grid(cols = vars(irrigation_type), rows = vars(case)) +
    labs(y = "Increase in payback period (years)", x = "weather station", fill = "Added costs") +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

increase.plot(paybackperiods_avg_all2, 90, "stn", "mrktyld", "irr.type")


delta_paybackperiods <- paybackperiods_avg_all2 |>
  mutate(davg_payback_min = round(avg_payback_min - avg_payback,
                                  digits = 0),
         davg_payback_med = round(avg_payback_med - avg_payback,
                                  digits = 0),
         davg_payback_max = round(avg_payback_max - avg_payback,
                                  digits = 0),
         abovethrshld_min = avg_payback_min > 20 & avg_payback < 20,
         abovethrshld_med = avg_payback_med > 20 & avg_payback < 20,
         abovethrshld_max = avg_payback_max > 20 & avg_payback < 20)


plot.change <- function(dataset, mrkt_yld, 
                  stn_col,
                  mrkt_yld_col,
                  irr_col){
  pd  <- position_dodge(width = 0.8)
  
  dataset <- dataset |>
    filter(.data[[mrkt_yld_col]] == mrkt_yld) |>
    mutate(
      stn_code = factor(.data[[stn_col]],
                        levels = c("S", "NG", "HCC", "EP")),
      irrigation_type = factor(.data[[irr_col]],
                               levels = c("pivot I",
                                          "pivot II",
                                          "hose reel + sprinkler",
                                          "hose reel + boom cart")
      )
    )
  
  common_margin <- margin(5, 5, 5, 28)
  
  plot_min <- ggplot(dataset, aes(x = stn_code,
                                  y=davg_payback_min,
                                  fill = soil,
                                  )) +
    geom_col(position = pd,
             width = 0.7) +
    geom_hline(yintercept = 0,
               linewidth = 0.2) +
    geom_text(aes(label = ifelse(dataset$abovethrshld_min == TRUE, "*",""),
                  group = soil),
              position = pd,
              vjust = -0.5,
              size = 5) +
    facet_grid(cols = vars(irrigation_type)) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    theme_minimal() + 
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = common_margin) +
    guides(fill = "none")
  
  plot_med <- ggplot(dataset,
                     aes(x = stn_code,
                         y=davg_payback_med,
                         fill = soil)) +
    geom_col(position = pd,
             width = 0.7) +
    geom_hline(yintercept = 0,
               linewidth = 0.2) +
    geom_text(aes(label = ifelse(dataset$abovethrshld_med == TRUE, "*",""),
                  group = soil),
              position = pd,
              vjust = -0.5,
              size = 5) +
    facet_grid(cols = vars(irrigation_type)) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    labs(y = "Increase in payback period (years)") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          plot.margin = common_margin) +
    guides(fill = "none")
  
  plot_max <- ggplot(dataset,
                     aes(x = stn_code,
                         y=davg_payback_max,
                         fill = soil)) +
    geom_col(position = pd,
             width = 0.7) +
    geom_hline(yintercept = 0,
               linewidth = 0.2) +
    geom_text(aes(label = ifelse(dataset$abovethrshld_max == TRUE, "*",""),
                  group = soil),
              position = pd,
              vjust = -0.5,
              size = 5) +
    facet_grid(cols = vars(irrigation_type)) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    labs(x = "Station Code",
         fill = "none") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.position = "bottom",
          plot.margin = common_margin) +
    guides(fill = "none")
  
  aligned <- cowplot::align_plots(plot_min, plot_med, plot_max, align = "v", axis = "l")
  
  p_stack <- cowplot::plot_grid(
    aligned[[1]], aligned[[2]], aligned[[3]],
    ncol = 1,
    rel_heights = c(1, 1, 1),
    labels = c("Low-cost", "Mid-cost", "High-cost"),
    label_size = 12,
    label_x = 0.02,
    label_y = 0.98,
    hjust = -0.65,
    vjust = 1
  )
  
  final_plot <- ggdraw() +
    cowplot::draw_label("Increase in Payback Period from Environmental Costs",
                        x = 0.02, y = 0.98, hjust = 0, vjust = 1, size = 14) +
    cowplot::draw_label(sprintf("Marketable Yield %s%% of Total Yield", mrkt_yld),
                        x = 0.02, y = 0.94, hjust = 0, vjust = 1, size = 11) +
    cowplot::draw_plot(p_stack, y = 0, height = 0.90)
  
  save_plot(
    filename = sprintf("temp/plots20260122/payback_delta_by_cost%d.png",
                       mrkt_yld),
    plot = final_plot,      # whatever object ggdraw() + draw_plot() returns
    base_width = 22,             # cm or inches (see units)
    base_height = 18,
    units = "cm",
    dpi = 300
  )
  
  final_plot
  
}

plot.change(delta_paybackperiods, 70, "stn", "mrktyld", "irr.type")


plot3 <- function(dataset, mrkt_yld, 
                  stn_col,
                  mrkt_yld_col,
                  irr_col){
  thr <- 20
  pd  <- ggplot2::position_dodge(width = 0.8)
  
  df_cross <- dataset |>
    dplyr::filter(.data[[mrkt_yld_col]] == mrkt_yld) |>
    dplyr::mutate(
      stn_code = factor(.data[[stn_col]], levels = c("S","NG","HCC","EP")),
      irrigation_type = factor(.data[[irr_col]],
                               levels = c("pivot I","pivot II","hose reel + sprinkler","hose reel + boom cart")
      )
    ) |>
    dplyr::transmute(
      stn_code, soil, irrigation_type,
      base = avg_payback,
      Low  = avg_payback_min,
      Mid  = avg_payback_med,
      High = avg_payback_max
    ) |>
    tidyr::pivot_longer(cols = c(Low, Mid, High), names_to = "case", values_to = "payback") |>
    dplyr::mutate(
      delta_years = payback - base,
      crosses_20  = base <= thr & payback > thr,   # <-- the key flag
      case = factor(case, levels = c("Low","Mid","High"))
    )
  
  ggplot2::ggplot(df_cross, aes(x = stn_code, y = delta_years, fill = soil)) +
    geom_col(position = pd, width = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_point(                                   # mark ONLY the crossings
      data = df_cross |> dplyr::filter(crosses_20),
      aes(group = soil),
      position = pd,
      shape = 8, size = 2, fill = "red", colour = "red"
    ) +
    facet_grid(rows = ggplot2::vars(case),
                        cols = ggplot2::vars(irrigation_type),
                        scales = "free") +
    labs(title = "Increase in Payback Period",
                  subtitle = sprintf("From Costs of Environmental Impacts\nMarketable Yield %s%% of Total Yield", mrkt_yld),
      y = "Increase in payback period (years)",
      x = "weather station",
      fill = "Soil"
    ) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
}

plot3(paybackperiods_avg_all2, 90, "stn", "mrktyld", "irr.type")
