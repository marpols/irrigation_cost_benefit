# All costs in $ CAD
# total.asset - $/ha.
# ownership - $/ha./year
# operation - $/ha./year

#marketable yields from total yield

market_yield_low <- 0.70 

market_yield_high <- 0.90

market_price <- 400.92 # $CAD/t.

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
payback.period <- function(irr_type, dataset, mrkt_yield, soil_name, stn){
  
  irrigation <- irrigation.costs()
  irr <- irrigation[[match(sprintf("%s", irr_type),
                           names(irrigation))]]  
  
  data <- dataset |> filter(soil == soil_name, stn_code == stn)
  
  initial_capital <- irr$total.asset
  operating_costs <- data[[irr$type]]
  annual_gross <- data[[sprintf("Gross Benefit, %s", mrkt_yield)]]
  
  annual_ncb <- sum(net.cash.benefit(annual_gross, operating_costs))
  return(initial_capital / (annual_ncb / length(annual_gross)))
}

net.cash.benefit <- function(annual_gross, annual_operation){
  annual_gross - annual_operation
}


