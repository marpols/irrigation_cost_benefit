make.climate.plots <- function(usm){
  
  station <- get.weather.station(usm)
  
  total_climate <- get.longterm.climate(usm)
  
  #monthly and yearly averages
  monthly_avgs <- get.monthly.avgs(total_climate)
  gs_summary <- get.summary(monthly_avgs, c(5:9))
  julaug_summary <- get.summary(monthly_avgs, c(7,8))
  
  #historical averages
  gs_hist_avg <- longterm.avg(gs_summary)
  julaug_hist_avg <- longterm.avg(julaug_summary)
  
  colours <- list(precip = c("#EFFC00", "#00D4FF","#090979"),
               temp = c("#00B1F7", "#FFD000", "#FF0000"))
  yvars <- c("precipitation (mm)", "GDD (base 5\u00B0C)")
  
  gs_precip_plot <- plot.climate(gs_summary, gs_hist_avg,
                                 precip.cum, station, colours[[1]],
                                "Cumulative Growing Season Precipitation",
                                yvars[1], 0)
  gs_precip_plot
  julaug_precip_plot <- plot.climate(julaug_summary, julaug_hist_avg,
                                    precip.cum, station, colours[[1]],
                                    "Cumulative July and August Precipitation",
                                    yvars[1], 0)
  julaug_precip_plot
  gs_temp_plot <- plot.climate(gs_summary, gs_hist_avg,
                               GDD, station, colours[[2]],
                               "Cumulative Growing Season GDD",
                               yvars[2], 1300)
  gs_temp_plot
  julaug_temp_plot <- plot.climate(julaug_summary, julaug_hist_avg,
                                  GDD, station, colours[[2]],
                                  "Cumulative July and August GDD",
                                  yvars[2], 700)
  julaug_temp_plot
}

make.plots <- function(sims_s, sims_ns, climfile) {
  yields <- yields.stress.nostress(sims_s, sims_ns)
  # monthly_summary <-
  # gs_summary <-
  # julaug_summary <-
  # cwr_deficits <-
  # gs_hist_avg <-
  # julaug_hist_avg <-
}

#climate plots
plot.mean.gs.climate <- function(dataset, variable, title, ylable, colour) {
  data <- if("period" %in% names(dataset)) 
    dataset |> filter(period == "GS") else dataset |> filter(mo %in% 6:9)
  data$stn_code <- lapply(data$station, get.stn.code) |> unlist()
  
  data$stn_code  <- factor(data$stn_code, 
                           levels = c("S", "NG", "HCC", "EP"))
  
  climplot <- ggplot(data, aes(x = stn_code, y = .data[[variable]])) +
    geom_boxplot(fill = colour) +
    labs(
      title = title,
      x = "weather station",
      y = ylable
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text = element_text(size = 16)
    )
  
  ggsave(sprintf("%s.png", variable), 
         plot = climplot,
         path = outdir,
         width = 17,
         height = 9.5,
         units = "cm")
  
  climplot
}



ggsave(
  sprintf("total_yield_gain_%s.png", mrkt_yld),
  plot = plot,
  path = outdir,
  width = 20,
  height = 15,
  units = "cm"
)

#historical climate
plot.climate <- function(data, hist_data, variable, station, 
                         colours, subtitle, y_lable, y_min){
  var_name <- as_name(ensym(variable))
  max <- max(data[[var_name]])
  mid <- hist_data[[var_name]]
  
  precip_plot <- ggplot(data, aes(x = ian, 
                                  y = {{variable}}, 
                                  fill = {{variable}})) +
    geom_col() +
    scale_fill_gradient2(low = colours[1], mid = colours[2], high = colours[3],
                         midpoint = mid) +
    geom_hline(data = hist_data, aes(yintercept = {{variable}}), linewidth = 0.8) +
    annotate("text", x = min(data$ian), y = mid, label = "20 year Average", 
             hjust = 0.4, vjust = -0.5, color = "black", size = 3) +
    scale_x_continuous(breaks = seq(min(data$ian), max(data$ian), by = 1)) +
    scale_y_continuous(breaks = seq(0, ceiling(max / 100) * 100, by = 100)) +
    coord_cartesian(ylim = c(y_min, NA)) +
    labs(title = {{station}},
         subtitle = subtitle, 
         y = y_lable,
         x = "year",
         fill = "") +
    guides(fill = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  precip_plot
}



make.box.plot <- function(data, hist, type, div, span = ""){
  precip_summary <- data |>
    group_by(year = ian) |>
    summarise(
      min_precip = min(precip.cum, na.rm = TRUE),
      max_precip = max(precip.cum, na.rm = TRUE),
      .groups = "drop"
    )
  
  precip_summary <- precip_summary |>
    mutate(mean_precip = (min_precip + max_precip) / 2)
  
  gdd_summary <- data |>
    group_by(year = ian) |>
    summarise(
      min_gdd = min(GDD, na.rm = TRUE),
      max_gdd = max(GDD, na.rm = TRUE),
      .groups = "drop"
    )
  
  hist <- mean(hist$precip.cum)
  
  if(type == "precip"){
    yield_box_plot <- ggplot(data) +
      geom_hline(aes(yintercept = hist/div), linewidth = 0.8) +
      geom_line(data = precip_summary, 
                aes(x = year, y = mean_precip/div),
                color = "#090979",
                linewidth = 0.5) +
      geom_ribbon(data = precip_summary,
                  aes(x = year,
                      ymin = min_precip/div,
                      ymax = max_precip/div),
                  alpha = 0.5, fill = "#00B1F7") +
      geom_boxplot(aes(x = ian,
                       y = `Yield, stress`,
                       group = ian),
                   fill = "#5CB564") +
      geom_boxplot(aes(x = ian,
                       y = `Yield, no stress`,
                       group = ian),
                   fill = "lightgreen", position = "dodge") +
      scale_y_continuous(
        sec.axis = sec_axis(~ . * div, name = "Precipitation (mm)")
      ) +
      labs(title = "Simulated Yield",
           subtitle = sprintf("Cumulative Precipitation, %s", span), 
           y = "Yield (t/ha.)",
           x = "Year",
           fill = "") +
      theme_minimal()
  } else {
    yield_box_plot <- ggplot(data) +
      geom_hline(aes(yintercept = hist/div)) +
      geom_ribbon(data = gdd_summary, aes(x = year, ymin = min_gdd/div,
                                          ymax = max_gdd/div)) +
      geom_boxplot(aes(x = ian,
                       y = `Yield, stress`,
                       group = ian)) +
      theme_minimal()
  }

  yield_box_plot
}

make.box.plot.soil <- function(data, hist, div, 
                          span = "", group = ""){
  
  data <- data |> filter(soil == toupper(group))
  hist <- mean(hist$precip.cum)
  
  precip_summary <- data |>
    group_by(year = ian) |>
    summarise(
      min_precip = min(precip.cum, na.rm = TRUE),
      max_precip = max(precip.cum, na.rm = TRUE),
      .groups = "drop"
    )
  
  precip_summary <- precip_summary |>
    mutate(mean_precip = (min_precip + max_precip) / 2)
  
  yield_box_plot <- ggplot(data) +
    geom_hline(aes(yintercept = hist/div), linewidth = 0.8) +
    geom_line(data = precip_summary, 
              aes(x = year, y = mean_precip/div),
              color = "#090979",
              linewidth = 0.5) +
    geom_ribbon(data = precip_summary,
                aes(x = year,
                    ymin = min_precip/div,
                    ymax = max_precip/div),
                alpha = 0.5, fill = "#00B1F7") +
    geom_boxplot(aes(x = ian,
                     y = `Yield, stress`,
                     group = ian),
                 fill = "#5CB564") +
    scale_y_continuous(
      sec.axis = sec_axis(~ . * div, name = "Precipitation (mm)")
    ) +
    labs(title = sprintf("Simulated Yield - %s", group),
         subtitle = sprintf("Cumulative Precipitation, %s", span), 
         y = "Yield (t/ha.)",
         x = "Year",
         fill = "") +
    theme_minimal()
  
  yield_box_plot
}

make.box.plot.station <- function(data, hist, div, 
                               span = "", group = ""){
  
  data <- data |> filter(station == toupper(group))
  hist <- hist |> filter(station ==  toupper(group))
  hist <- mean(hist$precip.cum)
  
  yield_box_plot <- ggplot(data) +
    geom_hline(aes(yintercept = hist/div), linewidth = 0.8) +
    geom_line(aes(x = ian, y = precip.cum/div),
              color = "#090979",
              linewidth = 0.8) +
    geom_boxplot(aes(x = ian,
                     y = `Yield, stress`,
                     group = ian),
                 fill = "#5CB564") +
    scale_y_continuous(
      sec.axis = sec_axis(~ . * div, name = "Precipitation (mm)")
    ) +
    labs(title = sprintf("Simulated Yield - %s", group),
         subtitle = sprintf("Cumulative Precipitation, %s", span), 
         y = "Yield (t/ha.)",
         x = "Year",
         fill = "") +
    theme_minimal()
  
  yield_box_plot
}

make.cwr.plot <- function(data, hist, div, 
                          span = "", group = ""){
  data <- if(group == "") data else data |> filter(station == toupper(group))
  data$CWR <- data$CWR * -1
  
  data_summary <- data |>
    group_by(year = ian) |>
    summarise(
      mean_cwr = mean(CWR, na.rm = TRUE),
      .groups = "drop"
    )
  min <- min(data_summary$mean_cwr)
  mid <- mean(data_summary$mean_cwr)
  max <- max(data_summary$mean_cwr)
  
  cwr.plot <- ggplot(data_summary, aes(x = year, y = mean_cwr, fill = mean_cwr)) +
    geom_col() +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFD000", high = "#00B1F7",
                         midpoint = mid)  +
    scale_y_continuous(breaks = seq(-100, 150, by  = 50)) +
    labs(title = sprintf("Crop Water Deficit/Surplus - %s", group),
         subtitle = sprintf("%s", span), 
         y = "Water deficit (mm)",
         x = "Year",
         fill = "Deficit (mm)") +
    theme_minimal()
  cwr.plot
  
}

make.mean.cwr.plot <- function(data, year){
  
  data$CWR <- data$CWR * -1
  
  
  data_summary <- data |>
    group_by(stn_code, soil, ian) |>
    summarise(
      mean_cwr = mean(CWR, na.rm = TRUE),
      .groups = "drop"
    )
  
  data_summary <- data_summary |> filter(ian == year)
  
  data_summary$stn_code <- factor(data_summary$stn_code, 
                             levels = c("S", "NG", "HCC", "EP"))
  
  min <- min(data_summary$mean_cwr)
  mid <- mean(data_summary$mean_cwr)
  max <- max(data_summary$mean_cwr)
  
  cwr.plot <- ggplot(data_summary, aes(x = stn_code, y = mean_cwr, fill = mean_cwr)) +
    geom_col(position = "dodge") +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFD000", high = "#00B1F7",
                         midpoint = mid)  +
    scale_y_continuous(breaks = seq(-100, 150, by  = 50)) +
    labs(title = sprintf("Crop Water Deficit/Surplus - %d", year),, 
         y = "mm",
         x = "Weather Station",
         fill = "Deficit (mm)") +
    theme_minimal() +
    facet_grid(cols = vars(soil))
  cwr.plot
  
}


cost.v.gains <- function(data,
                         rotation = 1,
                         stn,
                         soil_name){
  #cost v gross benefit plots per rotation
  #(assuming no irrigation use on two non-potato years)
  
  soil_stn_df <- gs_cost_benefit |> filter(stn_code == stn,
                                           soil == soil_name)
  r <- sprintf("rot%d", rotation)
  
  cost_col_1 <- sprintf("%scosts.cum, pivot I", r)
  cost_col_2 <- sprintf("%scosts.cum, pivot II", r) 
  cost_col_3 <- sprintf("%scosts.cum, hose reel + sprinkler", r) 
  cost_col_4 <- sprintf("%scosts.cum, hose reel + boom cart", r) 
  
  gross_col_low <- sprintf("%sgross.cum, low", r)
  gross_col_high <- sprintf("%sgross.cum, high", r)

  
  ggplot(soil_stn_df, aes(x = ian)) +
    geom_step(aes(y = .data[[gross_col_high]]/1000,
                  colour = "Gross Benefit, 90%"),
              linewidth = 1.2) +
    geom_step(aes(y = .data[[gross_col_low]]/1000,
                  colour = "Gross Benefit, 70%"),
              linewidth = 1.2) +
    geom_step(aes(y = .data[[cost_col_1]]/1000,
                  colour = "Pivot I"),
              linewidth = 0.7) +
    geom_step(aes(y = .data[[cost_col_2]]/1000,
                  colour = "Pivot II"),
              linewidth = 0.7) +
    geom_step(aes(y = .data[[cost_col_3]]/1000,
                  colour = "Hose Reel, Sprinkler"),
              linewidth = 0.7) +
    geom_step(aes(y = .data[[cost_col_4]]/1000,
                  colour = "Hose Reel, Boom Cart"),
              linewidth = 0.7) +
    scale_color_manual(values = c("Gross Benefit, 70%" = "#9ACD32",
                                  "Gross Benefit, 90%" = "#367c2b",
                                  "Pivot I" = "#fd5c63",
                                  "Pivot II" = "#9e1b32",
                                  "Hose Reel, Sprinkler" = "#00BFFF",
                                  "Hose Reel, Boom Cart" = "#1C39BB")) +
    scale_x_continuous(name = "Year",
                       breaks = seq(min(data$ian), max(data$ian), by = 1)) +
    scale_y_continuous(name = "CAD $ thousands / ha.",
                       breaks = seq(5,30, by = 5)) +
    labs(title = "Cumulative Costs and Gross Benefit 2001 - 2024",
         subtitle = sprintf("%s, %s\n Rotation begins in 200%d",
                            stringr::str_to_title(unique(soil_stn_df$station)),
                            soil_name,
                            rotation),
         colour = "") +
    guides() +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank())
  
  ggsave(sprintf("costbenefit_%s_%s_rot%d.png", stn, soil_name, rotation),
         path = file.path(outdir, "rotations"),
         width = 20,
         height = 15,
         units = "cm")
  
}

cost.gains.all.years <- function(dataset, stn, soil_name){
  
  soil_stn_df <- dataset |> filter(stn_code == stn,
                                           soil == soil_name)
  #cost v gross benefit plots
  #assuming irrgation use each year 
  #(farmer has at least one field in potato each year)
  
  print(paste0(stn, "_", soil_name))
  
  plot <- ggplot(soil_stn_df, aes(x = ian)) +
    geom_step(aes(y = `gross.cum, high`/1000,
                  colour = "Gross Benefit, 90%"),
              linewidth = 1.2) +
    geom_step(aes(y = `gross.cum, low`/1000,
                  colour = "Gross Benefit, 70%"),
              linewidth = 1.2) +
    geom_step(aes(y = `costs.cum, pivot I`/1000,
                  colour = "Pivot I"),
              linewidth = 0.7) +
    geom_step(aes(y = `costs.cum, pivot II`/1000,
                  colour = "Pivot II"),
              linewidth = 0.7) +
    geom_step(aes(y = `costs.cum, hose reel + sprinkler`/1000,
                  colour = "Hose Reel, Sprinkler"),
              linewidth = 0.7) +
    geom_step(aes(y = `costs.cum, hose reel + boom cart`/1000,
                  colour = "Hose Reel, Boom Cart"),
              linewidth = 0.7) +
    scale_color_manual(values = c("Gross Benefit, 70%" = "#9ACD32",
                                  "Gross Benefit, 90%" = "#367c2b",
                                  "Pivot I" = "#fd5c63",
                                  "Pivot II" = "#9e1b32",
                                  "Hose Reel, Sprinkler" = "#00BFFF",
                                  "Hose Reel, Boom Cart" = "#1C39BB")) +
    scale_x_continuous(name = "Year",
                       breaks = seq(min(data$ian), max(data$ian), by = 1)) +
    scale_y_continuous(name = "CAD $ thousands / ha.",
                       breaks = seq(5,30, by = 5)) +
    labs(title = "Cumulative Costs and Gross Benefit 2001 - 2024",
         subtitle = sprintf("%s, %s", stringr::str_to_title(unique(soil_stn_df$station)),
                            soil_name),
         colour = "") +
    guides() +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank())
  
  ggsave(sprintf("costbenefit_%s_%s.png", stn, soil_name),
         plot = plot,
         path = outdir,
         width = 20,
         height = 15,
         units = "cm")
}


yield.gain.plot <- function(dataset){
  #dataset = gs_data
  
  data_summary <- dataset |> group_by(stn_code, soil) |>
    reframe(total.gain = sum(Gains.total))
  
  data_summary$stn_code  <- factor(data_summary$stn_code, 
                         levels = c("S", "NG", "HCC", "EP"))
  
  plot <- ggplot(data_summary) +
    geom_col(aes(x = stn_code, y = total.gain, fill = soil),
             position = "dodge") +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    scale_y_continuous(breaks = 
                         seq(0,200, by = 50),
                       sec.axis = 
                         sec_axis(~.* 8.92, name =
                                    expression("Cwt" %.%"ac."^{-1}))) +
    coord_cartesian(ylim = c(0,200)) +
    labs(title = "Total Yield Gain 2001 - 2024",
         x = "weather station",
         y = expression(t %.% ha.^-1),
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
  plot
  
  ggsave("total_yield_gains.png", 
         plot = plot,
         path = outdir,
         width = 20,
         height = 15,
         units = "cm")
}

marketable.yield.gain.plot <- function(dataset, mrkt_yld){
  
  col <- grep(mrkt_yld, names(gain_by_soilstn), value = T)
  
  plot <- ggplot(dataset) +
    geom_col(aes(x = stn_code, y = .data[[col]], fill = soil),
             position = "dodge") +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    scale_y_continuous(breaks = seq(0,200, by = 50),
                       sec.axis = sec_axis(~.* 8.92, name = "Cwt / ac.")) +
    coord_cartesian(ylim = c(0,200)) +
    labs(title = "Total Marketable Yield Gain 2001 - 2024",
         subtitle = sprintf("Marketable Yield %s of Total Yield",
                            if(mrkt_yld == "low") "70%" else "90%"),
         x = "weather station",
         y = "t/ha.",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))
  plot
  
  ggsave(sprintf("total_yield_gain_%s.png", mrkt_yld), 
         plot = plot,
         path = outdir,
         width = 20,
         height = 15,
         units = "cm")
}

annual.net.benefit.plot <- function(dataset, irr_name, mrkt_yld){
  
  x <- grep_custom(paste(irr_name, mrkt_yld, sep = " "), names(dataset))
  col <- names(dataset)[x]
  
  plot <- ggplot(dataset, aes(x = stn_code, fill = soil)) +
    geom_col(aes(y = .data[[col]]),
             position = "dodge") +
    geom_hline(yintercept = 0,
               linewidth = 0.5) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    coord_cartesian(ylim = c(-550, 1500)) +
    labs(title = "Annual Net Benefit",
         subtitle = sprintf("%s Irrigation\n Marketable Yield %s of Total Yield",
                            irr_name,
                            if(mrkt_yld == "low") "70%" else "90%"),
         y = "$/ha./year",
         x = "weather station",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(sprintf("annual_net_benefit_%s_%s.png",irr_name, mrkt_yld), 
         plot = plot,
         path = outdir,
         width = 20,
         height = 15,
         units = "cm")

}

payback.period.plot <- function(dataset, mrkt_yld){
  
  dataset <- dataset |> filter(market_yield == mrkt_yld)
  dataset$stn_code <- factor(dataset$stn_code, 
                             levels = c("S", "NG", "HCC", "EP"))
  dataset$irrigation_type <- factor(dataset$irrigation_type,
                                    levels = c("pivot I",
                                               "pivot II",
                                               "hose reel + sprinkler",
                                               "hose reel + boom cart"))
  
  plot <- ggplot(dataset, aes(x = stn_code, fill = soil)) +
    geom_col(aes(y = payback_period),
             position = "dodge") +
    geom_hline(yintercept = 24,
               linewidth = 0.5,
               colour = "red") +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(title = "Payback Period",
         subtitle = sprintf("Marketable Yield %s of Total Yield",
                            if(mrkt_yld == "low") "70%" else "90%"),
         y = "Years",
         x = "weather station",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_grid(cols = vars(irrigation_type))
  
  ggsave(sprintf("payback_period_%s.png", mrkt_yld), 
         plot = plot,
         path = outdir,
         width = 20,
         height = 15,
         units = "cm")
  
}

tokenize <- function(x) {
  x |>
    str_replace_all("([a-z])([A-Z])", "\\1 \\2") |>  # camelCase split
    str_replace_all("[^a-zA-Z0-9]+", " ") |>         # non-alphanumeric to space
    str_to_lower() |>
    str_split(" +") |>
    unlist()
}

contains_all_tokens <- function(text, phrase) {
  text_tokens <- tokenize(text)
  phrase_tokens <- tokenize(phrase)
  all(phrase_tokens %in% text_tokens)
}

grep_custom <- function(pattern, vector){
  lapply(vector,
        contains_all_tokens,
        pattern) |> unlist()
}

wb.plot1 <- function(dataset){
  #make wb plot for growing season - df = wb_summary
  data <- dataset |> filter(mo == "GS") 
  data$stn_code <- lapply(data$station, get.stn.code) |> unlist()
  
  data$stn_code  <- factor(data$stn_code, 
                             levels = c("S", "NG", "HCC", "EP"))

  #average
  mean_wb <- data |> 
    group_by(stn_code, soil) |>
    summarise(mean_gs_wb  = mean(WB, na.rm = T))
  
  mean_wb$stn_code <- factor(mean_wb$stn_code, 
                             levels = c("S", "NG", "HCC", "EP"))
  
  mean_plot <- ggplot(mean_wb, aes(x = stn_code, y = mean_gs_wb, fill = soil)) +
    geom_col(position = "dodge") +
    theme_minimal()+
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    labs(title = "Mean Growing Season Water Balance 2001 - 2024",
         x = "weather station",
         y = "mm",
         fill = "Soil") +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    coord_cartesian(ylim = c(0,100))
  mean_plot
  
  ggsave("gs_mean_wb.png", 
         plot =  mean_plot,
         path = outdir,
         width = 17,
         height = 11.5,
         units = "cm")
}

wb.plot2 <- function(dataset){
  #make monthly wb plot - df = wb_summary
  data <- dataset |> filter(!(mo == "GS"), year %in% c(2001, 2020))
  data$stn_code <- lapply(data$station, get.stn.code) |> unlist()
  
  data$stn_code  <- factor(data$stn_code, 
                           levels = c("S", "NG", "HCC", "EP"))
  
  #average
  mean_wb <- data |> 
    group_by(stn_code, soil) |>
    summarise(mean_gs_wb  = mean(WB, na.rm = T))
  
  mean_wb$stn_code <- factor(mean_wb$stn_code, 
                             levels = c("S", "NG", "HCC", "EP"))
  
  mean_plot <- ggplot(mean_wb, aes(x = stn_code, y = mean_gs_wb, fill = soil)) +
    geom_col(position = "dodge") +
    theme_minimal()+
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    labs(title = "Mean Growing Season Water Balance 2001 and 2020",
         x = "weather station",
         y = "mm",
         fill = "Soil") +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    coord_cartesian(ylim = c(0,100))
  mean_plot
  
  ggsave("gs_mean_wb_drought.png", 
         plot =  mean_plot,
         path = outdir,
         width = 17,
         height = 11.5,
         units = "cm")
}

wb.plot3 <- function(dataset){
  #drought year water balance
  data <- dataset |> filter(!(mo == "GS"), year == "2001")
  data$stn_code <- lapply(data$station, get.stn.code) |> unlist()
  
  data$stn_code  <- factor(data$stn_code, 
                           levels = c("S", "NG", "HCC", "EP"))
  
  ggplot(data, aes(x = mo, y = WB, fill = soil)) +
    geom_col(position = "dodge") +
    theme_minimal() +
    facet_grid(cols = vars(stn_code))
  
  
}

wb.plot4 <- function(){
  #make divided water balance plot - df = wb_summary
}

raw.plot <- function(dataset){
  #readily available water <- df = RAW
  data <- dataset |> filter(mo %in% 6:9)
  
  data$stn_code  <- factor(data$stn_code, 
                           levels = c("S", "NG", "HCC", "EP"))
  
  data_summary <- data |> group_by(soil, stn_code, mo) |>
    reframe(mean_raw_gs = mean(RAW))
  
  mean <- ggplot(data_summary, aes(x = mo, y = mean_raw_gs, fill = soil)) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = c(0, 50)) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    labs(title = "Mean Readily Available Water",
         subtitle = "2001 - 2024",
         x = "MOY",
         y = "mm",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)
    ) +
    facet_grid(cols = vars(stn_code))
  
  data2 <- data |> filter(year %in% c(2001, 2020))
  
  data_summary2 <- data2 |> group_by(soil, stn_code, mo) |>
    reframe(mean_raw_gs = mean(RAW))
  
  drought_mean <- ggplot(data_summary2, aes(x = mo, y = mean_raw_gs, fill = soil)) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = c(0, 50)) +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    labs(title = "Mean Readily Available Water",
         subtitle = "2001 and 2020",
         x = "MOY",
         y = "mm",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    facet_grid(cols = vars(stn_code))
  
  ggsave("mean_raw.png", 
         plot = mean,
         path = outdir,
         width = 17,
         height = 11.5,
         units = "cm")
  
  ggsave("mean_raw_drought.png", 
         plot = drought_mean,
         path = outdir,
         width = 17,
         height = 11.5,
         units = "cm")
  mean
  drought_mean
           
}

# irr_names <- c("Pivot I", 
#                "Pivot II", 
#                "Hose Reel + Boom Cart", 
#                "Hose Reel + Sprinkler")
# market_ylds <- c("low", "high")
# 
# combos <- expand.grid(
#   irrigation = irr_names,
#   market_yield = market_ylds,
#   stringsAsFactors = FALSE
# )



# plot_list <- list()
# for (i in 1:nrow(combos)){
#   plot_list[[paste(combos[i,1],combos[i,2], sep = ", ")]] <- annual.net.benefit.plot(annual_net_benefit, combos[i,1], combos[i,2]) 
# }






# #make costs vs gain plots
# 
# irrigation_types <- names(irrigation.costs())
# soil_types <- c("CTW","ARY","CLO")
# stations <- unique(cost_benefit$station)
# rotations <- 1:3
# market_yields <- c("low", "high")
# period <- unique(cost_benefit$period)
# 
# combos <- expand.grid(
#   irrigation = irrigation_types,
#   station = stations,
#   soil = soil_types,
#   rotation = rotations,
#   market_yield = market_yields,
#   period = period,
#   stringsAsFactors = FALSE
# )
# 
# make.individual.plots <- function(data, combo_df){
#   
#   for(i in 1:nrow(combo_df)){
#     r <- combo_df[i,]
# 
#     path <- file.path(outdir, r["period"], r["irrigation"], r["soil"])
#     fname <- paste(r["station"], r["market_yield"],r["rotation"],
#                    ".png", sep = "_")
#     message(sprintf("Creating: %s\n in %s", fname, path))
#     
#     make.dir(path)
#     
#     sub_df <- data |> filter(period == r[["period"]],
#                            station == r[["station"]],
#                            soil == r[["soil"]])
#     
#     plot <- cost.v.gains(sub_df, r[["rotation"]],
#                          r[["market_yield"]],
#                          r[["irrigation"]],
#                          r[["station"]],
#                          r[["soil"]])
#     
#     ggsave(fname, plot, path = path, width = 15, height = 10, units = "cm")
#     
#     message("✅Plot created and saved successfully")
#   }
# }
# 
# make.dir <- function(filepath){
#   if(!dir.exists(filepath)) make.dir(sub("/[^/]*$", "", filepath))
#   dir.create(filepath)
# }

# data <- cost_benefit |> filter(period == "GS", stn_code == "S", soil == "CTW")
# 
# make.box.plot(gs_data, overall_hist_gs,"precip", 10, "Growing Season")
# make.box.plot(julaug_data, overall_hist_julaug,"precip", 5, "July to August") 
# 
# make.box.plot.station(gs_data, overall_hist_gs, 10,
#                       "Growing Season", "Summerside")
# 
# make.box.plot.soil(gs_data, overall_hist_gs, 10,
#                       "Growing Season", "CTW")
# make.box.plot.soil(gs_data, overall_hist_gs, 10,
#                    "Growing Season", "ARY")
# make.box.plot.soil(gs_data, overall_hist_gs, 10,
#                    "Growing Season", "CLO")
# 
# make.cwr.plot(gs_data)
# 
# make.individual.plots(cost_benefit, combos)




# outdir <- "~/Research/Project - Irrigation Assessment/Figures - 20250709_CORRECTED"
# dir.create(outdir)
# 
# combos <- expand.grid(
#   stations = unique(gs_cost_benefit$stn_code),
#   soils = unique(gs_cost_benefit$soil),
#   stringsAsFactors = FALSE)
# 
# combos_rot <- expand.grid(
#   stations = unique(gs_cost_benefit$stn_code),
#   soils = unique(gs_cost_benefit$soil),
#   rotation = 1:3,
#   stringsAsFactors = FALSE)
# 
# for(i in 1:nrow(combos)){
#   cost.gains.all.years(gs_cost_benefit, combos[i,1], combos[i,2])
#}
# 
# for(i in 1:nrow(combos_rot)){
#   cost.v.gains(gs_cost_benefit,
#                combos_rot[i,3],
#                combos_rot[i,1],
#                combos_rot[i,2])
# }

plot.monthly <- function(dataset){
  data <- dataset |> filter(mo %in% c(6:9))
  data$stn_code <- lapply(data$station, get.stn.code) |> unlist()
  
  data$stn_code  <- factor(data$stn_code, 
                           levels = c("S", "NG", "HCC", "EP"))
  
  data_summary <- data |> group_by(mo, stn_code) |>
    reframe(mean_precip = mean(precip.cum))
  
  mid <- mean(data_summary$mean_precip)
  
  plot <- ggplot(data_summary, aes(x = mo,
                           y = mean_precip,
                           fill = mean_precip)) +
    geom_col() +
    scale_fill_gradient2(low = "#ED5353",
                        mid = "#3DC8FF",
                        high = "#0E3F99",
                        midpoint = mid) + 
    theme_minimal() +
    labs(title = "Mean Monthly Precipitation",
         x = "MOY",
         y = "mm") +
    guides(fill = "none") +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    facet_grid(cols = vars(stn_code))
  
  data2 <- data |> filter(ian %in% c(2001, 2020))
  
  data_summary2 <- data2 |> group_by(mo, stn_code) |>
    reframe(mean_precip = mean(precip.cum))
  
  plot2 <- ggplot(data_summary2, aes(x = mo,
                                   y = mean_precip,
                                   fill = mean_precip)) +
    geom_col() +
    scale_fill_gradient2(low = "#ED5353",
                         mid = "#3DC8FF",
                         high = "#0E3F99",
                         midpoint = mid) +
    coord_cartesian(ylim = c(0,120)) +
    theme_minimal() +
    labs(title = "Mean Monthly Precipitation Drought Years",
         x = "MOY",
         y = "mm") +
    guides(fill = "none") +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 18),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    facet_grid(cols = vars(stn_code))
  
  ggsave("montly_precip.png", 
         plot = plot,
         path = outdir,
         width = 17,
         height = 11.5,
         units = "cm")
  
  ggsave("monthly_precip_drought.png", 
         plot = plot2,
         path = outdir,
         width = 17,
         height = 11.5,
         units = "cm")
  
}

plot.yearly.precip <- function(dataset, stn){
  
  dataset$stn_code <- lapply(dataset$station, get.stn.code) |> unlist()
  
  # data$stn_code  <- factor(data$stn_code, 
  #                          levels = c("S", "NG", "HCC", "EP"))
  
  data <- dataset |> filter(stn_code == stn, period == "GS")
  
  hist_climate <- historical |> filter(period == "GS")
  hist_climate$stn_code <- lapply(hist_climate$station, get.stn.code) |> unlist()
  mid <- hist_climate |> filter(stn_code == stn)
  mid <- mid$precip.cum
  
  p <- ggplot(data, aes(x <- ian, y = precip.cum, fill = precip.cum)) +
    geom_col(position = "dodge") +
    scale_x_continuous(breaks = seq(min(data$ian), max(data$ian), by = 1)) +
    geom_hline(yintercept = mid) +
    scale_fill_gradient2(low = "#ED5353",
                         mid = "#3DC8FF",
                         high = "#0E3F99",
                         midpoint = mid) +
    coord_cartesian(ylim = c(0,600)) +
    labs(title = "Cumulative Yearly Growing Season Precipitation",
         subtitle = unique(data$station),
         x = year,
         y = "mm", 
         fill = "mm") +
    theme_minimal()
  
  
  ggsave(sprintf("cum_yearly_precip_%s.png", stn), 
         plot = p,
         path = outdir,
         width = 30,
         height = 15,
         units = "cm")
}

overall_yearly <- hist_annual |> group_by(ian, period) |>
  reframe(cum.precip = mean(precip.cum))
