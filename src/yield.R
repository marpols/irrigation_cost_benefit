get.yields <- function(sims){
  yields <- lapply(sims, function(x){
    max(x$pdsfruitfrais)
  })
  return(yields)
}

avg.yield <- function(sims){
  yields <- as.vector(get.yields(sims))
  
  mean <- mean(unlist(yields))
  return(mean)
}
  
calc.market.yield <- function(sim, percent = NULL){
  market_yield <- if(percent == "low") market_yield_low  else market_yield_high
  return(max(sim$pdsfruitfrais) * market_yield)
}

yield.difference <- function(sim_s, sim_ns){
  
  if (typeof(sim_s) == "list") {
    diff <- max(max(sim_ns$pdsfruitfrais) - max(sim_s$pdsfruitfrais),0)
  }else {
    diff <- max(sim_ns - sim_s,0)
  }
  return(diff)
}

yield.gain <- function(sim_s, sim_ns){
  return(mapply(yield.difference, sim_s, sim_ns))
}

yields.stress.nostress <- function(sims_s,sims_ns){
  stress <- get.yields(sims_s)
  no_stress <- get.yields(sims_ns)
  usm_name <- get.groups(sims_s)
  years <- get.years(sims_s)
  
  df <- data.frame(
    "group" <- usm_name,
    "year" <- years,
    "yield" <- stress,
    "yeild_noWS" <- no_stress,
    "yield_ctw" <- stress * 8.92,
    "yield_noWs_ctw" <- no_stress * 8.92
  )
  
  return(df)
}


sowing.date <- function(sim, format = "MMdd"){
  
}

harvest.date <- function(sim, format = "MMdd"){
  
}


