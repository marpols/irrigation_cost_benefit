get.climate <- function(usm){
  climfile <- read.csv(usm[[3]], sep="\t", header = F)
  names(climfile) <- c("station", 
                       "year", 
                       "month", 
                       "day", 
                       "doy", 
                       "min.temp", 
                       "max.temp", 
                       "solar.rad", 
                       "PET", 
                       "precip.", 
                       "windspeed", 
                       "vapour.pressure", 
                       "CO2")
  return(climfile)
}