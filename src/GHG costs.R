#costs of emissions, 2024 values

GHG_costs <- data.frame(
  "climate_N2O" = c(8.88, 22.20, 37.74),
  "health_N2O" = c(2.22, 4.44, 6.66),
  "ecosystem_NO3" = c(11.10,	26.64,	44.40),
  "health_NO3" = c(0.00, 2.22, 8.88)
)
row.names(GHG_costs) <- c("min", "med", "max")
