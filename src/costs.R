# All costs in $ CAD
# total.asset - $/ha.
# ownership - $/ha./year
# operation - $/ha./year

#marketable yields from total yield

market_yield_low <- 0.70 

market_yield_high <- 0.90

market_price <- 400.92 # $CAD/t.

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
  

calc.yearly.earnings <- function(mrkt_yld){
  return(mrkt_yld * market_price) 
}



