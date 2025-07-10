#processed simulation data
yield_data <- readRDS("data/hills_yield_all.RDS")
gs_data <- yield_data |> filter(period == "GS")
julaug_data <- yield_data |> filter(period == "julaug")

#historical averages
historical <- readRDS("data/historical_avgs.RDS")
overall_hist_gs <- historical |> filter(period == "GS")
overall_hist_julaug <- historical |> filter(period == "julaug")

#processed simulation data with all costs and earnings calculated
cost_benefit <- readRDS("data/Capital_Capital_all.RDS")
gs_cost_benefit <- cost_benefit |> filter(period == "GS")
julaug_cost_benefit <- cost_benefit |> filter(period == "julaug")

#daily weather data for each station
stn_hcc <- readRDS("data/HARRINGTON_CDA_CS_climatefile.RDS")
stn_ep <- readRDS("data/EAST_POINT_(AUT)_climatefile.RDS")
stn_s <- readRDS("data/SUMMERSIDE_climatefile.RDS")
stn_ng <- readRDS("data/NEW_GLASGOW_climatefile.RDS")

#data grouped by soil type
by_soil_CTW <- gs_cost_benefit |> filter(soil == "CTW")
by_soil_ARY <- gs_cost_benefit |> filter(soil == "ARY")
by_soil_CLO <- gs_cost_benefit |> filter(soil == "CLO")

#total yield gain 2001-2024 by soil type and weather station
gain_by_soilstn <- gs_cost_benefit |> group_by(soil, stn_code) |>
  summarise(total.yield.gain.high = sum(Gains.market.high),
            total.yield.gain.low = sum(Gains.market.low))

high_gain <- gain_by_soilstn[,1:3] |>
  pivot_wider(names_from = stn_code, values_from = total.yield.gain.high)

low_gain <- gain_by_soilstn[,c(1,2,4)] |>
  pivot_wider(names_from = stn_code, values_from = total.yield.gain.low)


annual_net_benefit <- gs_cost_benefit |> group_by(soil, stn_code) |>
  summarise(pivotI = sum(`costs.yearly, pivot I`),
            pivotII = sum(`costs.yearly, pivot II`),
            hoseReel_Sprinkler = sum(`costs.yearly, hose reel + sprinkler`),
            hoseReel_boomCart = sum(`costs.yearly, hose reel + boom cart`),
            gross_benefit_low = sum(`Gross Benefit, low`),
            gross_benefit_high = sum(`Gross Benefit, high`))

for (i in 3:6){
  col <- names(annual_net_benefit)[i]
  colname_low <- paste(col,"low", sep = ".")
  annual_net_benefit[[colname_low]] <- (annual_net_benefit[[7]] - annual_net_benefit[[i]])/24
  annual_net_benefit[[paste(col,"high", sep = ".")]] <- (annual_net_benefit[[8]] - annual_net_benefit[[i]])/24
}


