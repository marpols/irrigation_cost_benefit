# All costs in $ CAD
# total.asset - $/ha.
# ownership - $/ha./year
# operation - $/ha./year
# climate_N2O - $/Kg N/ha.
# health_N2O - $/Kg N/ha.
# ecosystem_NO3 - $/Kg N/ha.
# health_NO3 - $/Kg N/ha.

mp <- 400.92 # $CAD/t., 2024

irrcosts <- function(){
pivotI <- list(
  type = "pivot I",
  id = "piv1",
  total.asset = 7823,
  ownership= 523,
  operation = 331
)

pivotII <- list(
  type = "pivot II",
  id = "piv2",
  total.asset = 6325,
  ownership= 378,
  operation = 71
)

hose.reelI <- list(
  type = "hose reel + sprinkler",
  id = "hrspr",
  total.asset = 10301,
  ownership= 661,
  operation = 710
)

hose.reelII <- list(
  type = "hose reel + boom cart",
  id = "hrbc",
  total.asset = 10562,
  ownership= 631,
  operation = 269
)

list(
  "pivot I" = pivotI,
  "pivot II" = pivotII,
  "hose reel + sprinkler" = hose.reelI,
  "hose reel + boom cart" = hose.reelII
)

}

envcosts <- function(){
  
  data.table::data.table(
    "lvl" = c("min", "mid", "max"),
    "climate_N2O" = c(8.88, 22.20, 37.74),
    "health_N2O" = c(2.22, 4.44, 6.66),
    "ecosystem_NO3" = c(11.10,	26.64,	44.40),
    "health_NO3" = c(0.00, 2.22, 8.88)
  )
  
}
