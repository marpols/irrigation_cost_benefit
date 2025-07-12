calculate.spi <- function(df, period = 1, station){
  #period -> number of months to aggregate in the calculation
  
  df <- df[df$station %in% station, ]
  
  df_wide <- df[, c(2, 3, 4, 10)] |>
    mutate(day_label = paste0("d", jo)) |>
    select(-jo) |>
    pivot_wider(names_from = day_label, values_from = precip.)
  
  spi <- precintcon.spi.analysis(as.daily(df_wide), period = 4)
  
  spi_agr <- spi |>  group_by(year) |>
    summarise(spi = mean(spi, na.rm = TRUE, ))
  
  spi_agr
}

# Calculating OWS (optimum water supply)
# sub <- yields |> filter(period == "GS")
# model <- lm(`Yield, stress` ~ precip.cum + I(precip.cum^2), sub)
# coefs <- coef(model)
# 
# # Find critical point (where slope = 0)
# x_crit <- -coefs["precip.cum"] / (2 * coefs["I(precip.cum^2)"])
# y_crit <- predict(model, newdata = data.frame(precip.cum = x_crit))
# 
# cat("Critical point at x =", x_crit, "with y =", y_crit, "\n")