# All costs in $ CAD
# total.asset - $/ha.
# ownership - $/ha./year
# operation - $/ha./year

#marketable yields from total yield

market_yield_low <- 0.70 

market_yield_high <- 0.90

market_price <- 400.92 # $CAD/t., 2024

rotation_2001 <- seq(2001,2024, by  = 3)
rotation_2002 <- seq(2002,2024, by  = 3)
rotation_2003 <- seq(2003,2024, by  = 3)

irrigation.costs <- function(){
pivotI <- list(
  type = "pivot I",
  total.asset = 7823,
  ownership= 523,
  operation = 331
)

pivotII <- list(
  type = "pivot II",
  total.asset = 6325,
  ownership= 378,
  operation = 71
)

hose.reelI <- list(
  type = "hose reel + sprinkler",
  total.asset = 10301,
  ownership= 661,
  operation = 710
)

hose.reelII <- list(
  type = "hose reel + boom cart",
  total.asset = 10562,
  ownership= 631,
  operation = 269
)

costs <- list("pivot I" = pivotI, 
           "pivot II" = pivotII, 
           "hose reel + sprinkler" = hose.reelI,
           "hose reel + boom cart" = hose.reelII
           )

return(costs)

}

calc.yearly.costs <- function(yield_gain, type, threshold){
  irrigated <- irrigated(yield_gain)
  if (irrigated){
    costs <- type$ownership + type$operation
  } else {
    costs <- type$ownership
  }
  return(costs)
}

sum.total <- function(values){
  return(sum(unlist(values)))
}
  

calc.earnings <- function(mrkt_yld){
  return(mrkt_yld * market_price) 
}

calc.sic <- function(data, irr_system){
  OWS <- data$cetm #Optimal Water Supply - Max Cumulative ET
  precip <- data$precip_e #Cumulative effective precipitation
  UWC <- irr_system$operation #operational cost based on application of 152 mm
  AOC <- irr_system$ownership #ownership cost
  
  message(sprintf("💰 %s-%s %s Yearly Costs (Not including Capital Costs)",
                  unique(data$stn_code), unique(data$soil), irr_system$type))
  mapply(sic.eqn,
         OWS, precip, UWC, AOC)
}

sic.eqn <- function(OWS = 1, x = 1, UWC = 1, AOC = 1){
#Jiang et al., 2022 - Equation (1)

  # If ownership costs = yearly depreciation + interest
  # if(x < OWS){
  #   SIC <- ((OWS - x) / 152) * UWC + AOC
  #   message(sprintf("SIC = ((%f.2 - %f.2)/ 152) * %f.2 + %f.2\n    = %f.2", OWS, x, UWC, AOC, SIC))
  # } else {
  #   SIC <- AOC
  #   message(sprintf("SIC = %f.2", SIC))
  # }
  
  # If ownership is considered a one time expense
  if(x < OWS){
    SIC <- ((OWS - x) / 152) * UWC
    message(sprintf("SIC = ((%f.2 - %f.2)/ 152) * %f.2 + %f.2\n    = %f.2", OWS, x, UWC, AOC, SIC))
  } else {
    SIC <- 0
    message(sprintf("SIC = %f.2", SIC))
  }
  SIC
}



cum.costs <- function(df) {
  irrigation_systems <- irrigation.costs()
  
  for (irr in irrigation_systems){
    
    col_name <- sprintf("costs.yearly, %s", irr$type)
    df[col_name] <- df[, names(df) == irr$type]
    df[1, col_name] <- df[1, col_name] + irr$total.asset #If ownership is considered a one time expense

    cum_col_name <- sprintf("costs.cum, %s", irr$type)
    df[cum_col_name] <- cumsum(df[col_name])
    
    for (i in 1:3) {
      rot <- sprintf("rotation_200%d", i)
      
      yrly_col <- sprintf("rot%dcosts.yearly, %s", i, irr$type)
      
      df[df$ian %in% get(rot), yrly_col] <- df[(df$ian %in% get(rot)), names(df) == irr$type]
      #df[!(df$ian %in% get(rot)), yrly_col] <- irr$ownership #If ownership costs = yearly depreciation + interest
      df[!(df$ian %in% get(rot)), yrly_col] <- 0 #If ownership is considered a one time expense
      df[df$ian %in% get(rot), yrly_col][i] <- df[df$ian %in% get(rot), yrly_col][i] + irr$total.asset #If ownership is considered a one time expense
      
      if (i %in% 2:3) {
        prevyrs <- 1:(which(df$ian == get(rot)[1]) - 1)
        df[prevyrs, yrly_col] <- 0
      }
      
      cum_col <- sprintf("rot%dcosts.cum, %s", i, irr$type)
      df[cum_col] <- cumsum(df[yrly_col])
    }
  }
  return(df)
}

cum.gross.benefit <- function(df) {
  
  col_low <- "gross.cum, low"
  col_high <- "gross.cum, high"
  
  df[col_low] <- cumsum(df["Gross Benefit, low"])
  df[col_high] <- cumsum(df["Gross Benefit, high"])
  
  
  for (i in 1:3) {
    rot <- sprintf("rotation_200%d", i)
    
    yrly_col_low <- sprintf("rot%dgross.yearly, low", i)
    yrly_col_high <- sprintf("rot%dgross.yearly, high", i)
      
    df[df$ian %in% get(rot), yrly_col_low] <- df[(df$ian %in% get(rot)), "Gross Benefit, low"]
    df[df$ian %in% get(rot), yrly_col_high] <- df[(df$ian %in% get(rot)), "Gross Benefit, high"]
    df[!(df$ian %in% get(rot)), yrly_col_low] <- 0
    df[!(df$ian %in% get(rot)), yrly_col_high] <- 0
    
    if (i %in% 2:3) {
      prevyrs <- 1:(which(df$ian == get(rot)[1]) - 1)
      df[prevyrs, yrly_col_low] <- 0
      df[prevyrs, yrly_col_high] <- 0
    }
    
    cum_col_low <- sprintf("rot%dgross.cum, low", i)
    df[cum_col_low] <- cumsum(df[yrly_col_low])
    
    cum_col_high <- sprintf("rot%dgross.cum, high", i)
    df[cum_col_high] <- cumsum(df[yrly_col_high])
    
  }
  return(df)
}

get.cost.benefit <- function(data){
  
  cum_costs <- cum.costs(data)
  cum_gross_benefit <- cum.gross.benefit(data)
  
  df <- cum_costs |> left_join(cum_gross_benefit,
                  by = c("ian", "stn_code", "soil"),
                  suffix = c("", ".y")) |>
    select(-ends_with(".y"))
  
}

annual.net.benefit <- function(total_costs, total_gross, span){
  net_benefit <- (total_costs - total_gross)/span
}

#for one-time capital cost only (no yearly ownership)
payback.period <- function(irr_type, dataset, mrkt_yield,
                           soil_name, stn_code,
                           start, end,
                           em_costs = FALSE,
                           scenario = NA,
                           projections = FALSE){
  
  irrigation <- irrigation.costs()
  irr <- irrigation[[match(sprintf("%s", irr_type),
                           names(irrigation))]]  
  
  # data <- dataset |> filter(soil == soil_name, stn_code == stn)
  if(projections){
    data <- dataset |> dplyr::filter(soil == soil_name,
                                     ssp == stn_code,
                                     ian %in% seq(start,end)) 
  } else {
    data <- dataset |> dplyr::filter(soil == soil_name,
                                     stn == stn_code,
                                     ian %in% seq(start,end)) 
  }
  
  
  initial_capital <- irr$total.asset
  operating_costs <- data[[irr$type]]
  annual_gross <- data[[sprintf("Gross Benefit, %s", mrkt_yield)]]
  if(em_costs){
    emissions <- data[[sprintf("total.em.costs.%s", scenario)]]
  }else{
    emissions <- 0
  }
  annual_ncb2 <- sum(net.cash.benefit(annual_gross, operating_costs, emissions))
  return(initial_capital / (annual_ncb / length(annual_gross)))
}

net.cash.benefit <- function(annual_gross, annual_operation, em_costs){
  annual_gross - annual_operation - em_costs
}


#with dataframe - 21-01-2026

calc.irr.costs <- function(df){
  irr_costs <- irrigation.costs()
  for(irr in irr_costs){
    df[[irr$type]] <- df$app_irr * irr$operation/152
  }
  df
}

calc.total.costs <- function(df, start, end){
  irr_costs <- irrigation.costs()
  newdf <- df |>
    distinct(scenario)
  
  period <- df[which(df$ian %in% seq(start, end)),]
  
  for(irr in irr_costs){
    whole_period <- period |>
      group_by(scenario) |>
      summarise("{irr$type} total cost" := sum(.data[[irr$type]])) 
    whole_period[[glue::glue("{irr$type} total cost")]] <-
      whole_period[[glue::glue("{irr$type} total cost")]] +
      irr$total.asset
    newdf <- left_join(newdf, 
                       whole_period,
                       by = "scenario")
  }
  newdf
}

calc.em.costs <- function(df){
  df$cost.N2O.min <- pmax(df$changeN2O_30mm,0) * 
    (GHG_costs$climate_N2O[1] + GHG_costs$health_N2O[1])
  df$cost.N2O.med <- pmax(df$changeN2O_30mm,0) * 
    (GHG_costs$climate_N2O[2] + GHG_costs$health_N2O[2])
  df$cost.N2O.max <- pmax(df$changeN2O_30mm,0) * 
    (GHG_costs$climate_N2O[3] + GHG_costs$health_N2O[3])
  
  df$cost.NO3.min <- pmax(df$changeQles_30mm,0) * 
    (GHG_costs$ecosystem_NO3[1] + GHG_costs$health_NO3[1])
  df$cost.NO3.med <- pmax(df$changeQles_30mm,0) * 
    (GHG_costs$ecosystem_NO3[2] + GHG_costs$health_NO3[2])
  df$cost.NO3.max <- pmax(df$changeQles_30mm,0) * 
    (GHG_costs$ecosystem_NO3[3] + GHG_costs$health_NO3[3]) 
  
  df$total.em.costs.min <- df$cost.N2O.min + df$cost.NO3.min
  df$total.em.costs.med <- df$cost.N2O.med + df$cost.NO3.med
  df$total.em.costs.max <- df$cost.N2O.max + df$cost.NO3.max
  
  df
}

# all_data <- left_join(z, 
#                       irr_30mm[,c("yield", "ian", "scenario")],
#                       by = c("scenario", "ian"))
# 
all_data$annualgross90_irr30 <- all_data$yieldgain_30mm * market_yield_high * market_price
all_data$annualgross70_irr30 <- all_data$yieldgain_30mm * market_yield_low * market_price

averages <- all_data |>
    group_by(ssp, ian, soil) |>
    summarise(yieldgain = mean(yieldgain_30mm),
              changeQles_30mm = mean(changeQles_30mm_argmax),
              changeN2O_30mm = mean(changeN2O_30mm_argmax),
              "Gross Benefit, 90" = mean(annualgross90_irr30),
              "Gross Benefit, 70" = mean(annualgross70_irr30),
              app_irr = mean(app_irr))

averages <- calc.em.costs(averages)
  

soils <- unique(all_data$soil)
# stns <- unique(all_data$stn)
mys <- c(90,70)
irrtypes <- irrigation.costs()
ssps <- unique(all_data$ssp)
# 
start <- 2027
end <- 2047
envcosts <- FALSE
scen <- "min" #for environmental costs: min, mid, max. NULL for none
proj <- TRUE
#   
# combos <- expand.grid(soils,stns,mys,stringsAsFactors = FALSE)
combos <- expand.grid(soils,ssps, mys,stringsAsFactors = FALSE)
 
# paybackperiods <- data.frame("soil"=NA,"stn"=NA,"irr.type"=NA,"mrktyld"=NA,
                             # "years"=NA,"start"=NA, "end"=NA)

paybackperiods <- data.frame("soil"=NA,"ssp"=NA,"irr.type"=NA,"mrktyld"=NA,
"years"=NA,"start"=NA, "end"=NA)

data <- averages

for(irr in irrtypes){
  i <- 1
  data[[irr$type]] <- irr$operation/152 * data$app_irr
  while(i <= nrow(combos)){
    pp <- payback.period(irr$type,
                         data,
                         combos[i,3],
                         combos[i,1],
                         combos[i,2],
                         start,
                         end,
                         em_cost= envcosts,
                         scenario=scen,
                         projections = proj)
    paybackperiods <- rbind(paybackperiods,
                            c(combos[i,1],
                              combos[i,2],
                              irr$type,
                              combos[i,3],
                              pp,
                              start,end))
    i <- i + 1
  }
}

paybackperiods <- paybackperiods |> remove_missing()


# saveRDS(averages, "data/proj_modelavgs.RDS")
# saveRDS(paybackperiods, sprintf("data/paybackperiods_irr30mm_em%s_%d%d.RDS",scen,start,end))
saveRDS(remove_missing(paybackperiods),"data/paybackperiods_proj20272047.RDS")

paybackperiods_proj$years <- as.numeric(paybackperiods_proj$years)
paybackperiods_proj$avg_payback_min <- as.numeric(paybackperiods_projmin$years)
paybackperiods_proj$avg_payback_med <- as.numeric(paybackperiods_projmed$years)
paybackperiods_proj$avg_payback_max <- as.numeric(paybackperiods_projmax$years)

paybackperiods_proj$davg_payback_min <- 
  as.numeric(paybackperiods_proj$avg_payback_min) -
  as.numeric(paybackperiods_proj$years)
paybackperiods_proj$davg_payback_med <- 
  as.numeric(paybackperiods_proj$avg_payback_med) -
  as.numeric(paybackperiods_proj$years)
paybackperiods_proj$davg_payback_max <- 
  as.numeric(paybackperiods_proj$avg_payback_max) -
  as.numeric(paybackperiods_proj$years)


# paybackperiods <- bind_rows(paybackperiods_irr30mm_20012020,
#                             paybackperiods_irr30mm_20022021,
#                             paybackperiods_irr30mm_20032022, 
#                             paybackperiods_irr30mm_20042023,
#                             paybackperiods_irr30mm_20052024) |> 
#   remove_missing()
# paybackperiods[which(paybackperiods$soil == ""),] <- NA
# paybackperiods <- remove_missing(paybackperiods)
# 
# paybackperiods_em_min <- bind_rows(paybackperiods_irr30mm_emmin_20012020,
#                                    paybackperiods_irr30mm_emmin_20022021,
#                                    paybackperiods_irr30mm_emmin_20032022, 
#                                    paybackperiods_irr30mm_emmin_20042023,
#                                    paybackperiods_irr30mm_emmin_20052024) |> 
#   remove_missing() |> saveRDS("data/paybackperiods_em_min.RDS")
#   
# paybackperiods_em_med <- bind_rows(paybackperiods_irr30mm_emmed_20012020,
#                                   paybackperiods_irr30mm_emmed_20022021,
#                                   paybackperiods_irr30mm_emmed_20032022, 
#                                   paybackperiods_irr30mm_emmed_20042023,
#                                   paybackperiods_irr30mm_emmed_20052024) |> 
#   remove_missing() |> saveRDS("data/paybackperiods_em_med.RDS")
# 
# paybackperiods_em_max <- bind_rows(paybackperiods_irr30mm_emmax_20012020,
#                                   paybackperiods_irr30mm_emmax_20022021,
#                                   paybackperiods_irr30mm_emmax_20032022, 
#                                   paybackperiods_irr30mm_emmax_20042023,
#                                   paybackperiods_irr30mm_emmax_20052024) |> 
#   remove_missing() |> saveRDS("data/paybackperiods_em_max.RDS")
# 
# 
# paybackperiods_avg <- paybackperiods |>
#   group_by(soil,stn,irr.type, mrktyld) |>
#   summarise(avg_payback = mean(as.numeric(years)))|>
#   saveRDS("data/paybackperiods_avg.RDS")
# 
# paybackperiods_em_min_avg <- paybackperiods_em_min |>
#   group_by(soil,stn,irr.type, mrktyld) |>
#   summarise(avg_payback = mean(as.numeric(years)))|>
#   saveRDS("data/paybackperiods_em_min_avg.RDS")
# 
# paybackperiods_em_med_avg <- paybackperiods_em_med |>
#   group_by(soil,stn,irr.type, mrktyld) |>
#   summarise(avg_payback = mean(as.numeric(years)))|>
#   saveRDS("data/paybackperiods_em_med_avg.RDS")
# 
# paybackperiods_em_max_avg <- paybackperiods_em_max |>
#   group_by(soil,stn,irr.type, mrktyld) |>
#   summarise(avg_payback = mean(as.numeric(years))) |>
#   saveRDS("data/paybackperiods_em_max_avg.RDS")
# 
# 
# paybackperiods_em_max_avg$em_cost <- "high"
# paybackperiods_em_med_avg$em_cost <- "mid"
# paybackperiods_em_min_avg$em_cost <- "low"
# paybackperiods_avg$em_cost <- "none"
# 
# paybackperiods_avg_all2 <- bind_cols(paybackperiods_avg,
#                                     "avg_payback_min" = paybackperiods_em_min_avg$avg_payback,
#                                     "avg_payback_med" = paybackperiods_em_med_avg$avg_payback,
#                                     "avg_payback_max" = paybackperiods_em_max_avg$avg_payback)
# 
# saveRDS(paybackperiods_avg_all, "data/paybackperiods_avg_all2.RDS")
