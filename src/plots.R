yield_data <- readRDS("data/hills_yield_all.RDS")
gs_data <- yield_data |> filter(period == "GS")
julaug_data <- yield_data |> filter(period == "julaug")
historical <- readRDS("data/historical_avgs.RDS")
overall_hist_gs <- historical |> filter(period == "GS")
overall_hist_julaug <- historical |> filter(period == "julaug")


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

make.plots <- function(sims_s,sims_ns, climfile){
  yields <- yields.stress.nostress(sims_s,sims_ns)
  # monthly_summary <-
  # gs_summary <- 
  # julaug_summary <- 
  # cwr_deficits <- 
  # gs_hist_avg <-
  # julaug_hist_avg <-
}

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
  
  colours <- list(precip = c("#EFFC00", "#00D4FF","#090979"),
                  temp = c("#00B1F7", "#FFD000", "#FF0000"))
  
  cwr.plot <- ggplot(data_summary, aes(x = year, y = mean_cwr, fill = mean_cwr)) +
    geom_col() +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFD000", high = "#00B1F7",
                         midpoint = mid)  +
    scale_y_continuous(breaks = seq(-100, 150, by  = 50)) +
    labs(title = sprintf("Crop Water Deficit - %s", group),
         subtitle = sprintf("%s", span), 
         y = "Water deficit (mm)",
         x = "Year",
         fill = "Deficit (mm)") +
    theme_minimal()
  cwr.plot
  
}

make.box.plot(gs_data, overall_hist_gs,"precip", 10, "Growing Season")
make.box.plot(julaug_data, overall_hist_julaug,"precip", 5, "July to August") 

make.box.plot.station(gs_data, overall_hist_gs, 10,
                      "Growing Season", "Summerside")

make.box.plot.soil(gs_data, overall_hist_gs, 10,
                      "Growing Season", "CTW")

make.cwr.plot(gs_data)
