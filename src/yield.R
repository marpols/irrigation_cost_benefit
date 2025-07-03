get.yields <- function(sims){
  yields <- lapply(sims, function(x){
    max(x$pdsfruitfrais)
  })
}

avg.yield <- function(sims){
  yields <- as.vector(get.yields(sims))
  
  mean <- mean(unlist(yields))
}
  
calc.market.yield <- function(sim){
  return(max(sim$pdsfruitfrais) * market_yield)
}

yield.difference <- function(sim_s, sim_ns){
  if (typeof(sim_s) == "list") {
    diff <- max(sim_ns$pdsfruitfrais) - max(sim_s$pdsfruitfrais)
  }else {
    diff <- sim_ns - sim_s
  }
  return(diff)
}

yield.gain <- function(sim_s, sim_ns){
  return(mapply(yield.difference, sim_s, sim_ns))
}


