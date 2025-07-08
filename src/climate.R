get.climate <- function(usm){
  #usm <- usm_files item
  climfile <- read.csv(usm$paths[[3]], sep="\t", header = F)
  names(climfile) <- c("station", 
                       "ian", 
                       "mo", 
                       "jo", 
                       "jul", 
                       "min.temp", 
                       "max.temp", 
                       "solar.rad", 
                       "PET", 
                       "precip.", 
                       "windspeed", 
                       "vapour.pressure", 
                       "CO2")
  class(climfile) <- c("climate file", "data.frame")
  return(climfile)
}

get.gs.clim <- function(climfile, start_jul, end_jul){
  return(climfile[which(climfile$jul == start_jul) : 
                    which(climfile$jul == end_jul),]
         )
}

get.gs.sim <- function(sim, start_jul, end_jul){
  return(sim[which(sim$jul == start_jul) : 
                    which(sim$jul == end_jul),]
  )
}

get.jul.aug.clim <- function(climfile){
  jul_start <- which(climfile$month == 7 & climfile$day == 1)
  aug_end <- which(climfile$month == 8 & climfile$day == 30)
  return(climfile[jul_start:aug_end,])
}

get.jul.aug.sim <- function(sim){
  jul_start <- which(sim$mo == 7 & sim$jo == 1)
  aug_end <- which(sim$mo == 8 & sim$jo == 30)
  return(climfile[jul_start:aug_end,])
}

get.sowing.doy <- function(sim){
  min_cet <- min(sim$cet[sim$cet > 0])
  return(sim$jul[grep(min_cet,sim$cet)])
}

get.harvest.doy <- function(sim){
  max_cet <- max(sim$cet)
  return(sim$jul[min(grep(max_cet,sim$cet))])
}

get.cum.precip <- function(file){
  if(class(file) == "climate file"){
    cum <- sum(file$precip.)
  } else if (class(file) == "STICS simulation"){
    cum <- sum(file$precip)
  } else{
    stop("argument should be of class 'climate file' or 'STICS simulation")
  }
  return(cum)
}


get.avg.cum.precip <- function(file, period){
  
  mean(yields[yields$period == period, ]$precip.cum)
}



crop.co.etc <- function(sim){
  #from Jiang, Y., Ramsay, M., Meng, F., & Stetson, T. (2021). 
  # Characterizing potato yield responses to water supply in Atlantic 
  # Canada’s humid climate using historical yield and weather data: 
  # Implications for supplemental irrigation. Agricultural Water Management,
  # 255, 107047. https://doi.org/10.1016/j.agwat.2021.107047

  initiation <- matrix(data = c(c(5, 6, 0.8), c(15, 10, NA)),
                       nrow = 3, ncol = 2)
  development <- matrix(data = c(c(6, 7, 0.9), c(11, 10, NA)),
                        nrow = 3, ncol = 2)
  midseason <- matrix(data = c(c(7, 8, 1.15), c(11, 25, NA)),
                      nrow = 3, ncol = 2)
  lateseason <- matrix(data = c(c(8, 9, 0.75), c(26, 25, NA)),
                       nrow = 3, ncol = 2)
  
  
  calculate.etc <- function(stage){
    span <- sim[which(sim$mo == stage[1,1] & sim$jo == stage[1,2]):
                      which(sim$mo == stage[2,1] & sim$jo == stage[2,2]),]
    return((sum(span$et0))*stage[3,1])
  }
  
  stages <- list("initiation" = initiation,
                 "development" = development,
                 "midseason" = midseason,
                 "lateseason" = lateseason)
  
  etc <- lapply(stages, calculate.etc)
  
  return(sum(unlist(etc)))
}

