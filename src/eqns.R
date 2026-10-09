SIC <- function(SIR, UWC){
  (SIR/152) * UWC
}

CB <- function(grossb, SIC, envcost = 0){
  grossb - SIC - envcost
} 

PP <- function(UC, CBnet, years){
  UC/(CBnet/years)
}

envext <- function(N2O, NO3, climate, health_N2O, ecosystem, health_NO3){
  N2O*climate + N2O*health_N2O + NO3*ecosystem + NO3*health_NO3
}
