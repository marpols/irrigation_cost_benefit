irrigated <- function(yield_diff){
  return(yield_diff > 0)
}

calc.water.requirements <- function(sim, method){
  #from planting date to harvest
  
  min_cet <- min(sim$cet[sim$cet > 0])
  sowing <- grep(min_cet,sim$cet)
  max_cet <- max(sim$cet)
  harvest <- min(grep(max_cet,sim$cet))
  
  prepcip.PET <- function(){
    
    eff_precip <- sum(sim$precip[sowing:harvest])
    
    CWR_gs <- max_cet - eff_precip #full growing season
    
    jul_aug <- sim[which(sim$mo == 7 & sim$jo == 1):
                     which(sim$mo == 8 & sim$jo == 30),]
    
    jul_aug_cet <- jul_aug$cet[61] - jul_aug$cet[1]
    jul_aug_eff_precip <- sum(jul_aug$precip)
    
    CWR_jul_aug <- jul_aug_cet - jul_aug_eff_precip
  }
  
  root.zone <- function(){
    #root zone = 60cm
    root_zone <- 600 #mm
    
    #method 1 - simulated soil water volume
    FC_vol <- sim$HR_mm.1.[1] + sim$HR_mm.2.[1] + sim$HR.3.[1]
    FC_80 <- FC_vol * 0.8
    
    sim$root.zone.moisture <- sim$HR_mm.1.+sim$HR_mm.2.+sim$HR_mm.3.
    
    sim$root.zone.CRW <- pmax(FC_80 - sim$root.zone.moisture,0)
    
    CWR_gs_rs_v1 <- sum(sim$root.zone.CRW[sowing:harvest])
    
    jul_aug_rs <- sim[which(sim$mo == 7 & sim$jo == 1):
                         which(sim$mo == 8 & sim$jo == 30),]
    
    CWR_jul_aug_rs_v1 <- sum(jul_aug_rs$root.zone.CRW)
    
    #method 2 - simulated volumetric water content
    FC_1 <- sim$HR_vol_1_30[1]*0.8
    FC_2 <- sim$HR_vol_31_60[1]*0.8
    
    sim$CWR_1 <- pmax((FC_1 - sim$HR_vol_1_30) * (root_zone/2), 0)
    sim$CWR_2 <- pmax((FC_2 - sim$HR_vol_31_60) * (root_zone/2) , 0)
    
    CWR_gs_rs_v2 <- sum(sim$CWR_1[sowing:harvest]) + sum(sim$CWR_2[sowing:harvest])
    
    jul_aug_rs <- sim[which(sim$mo == 7 & sim$jo == 1):
                        which(sim$mo == 8 & sim$jo == 30),]
    
    CWR_jul_aug_rs_v2 <- sum(jul_aug_rs$CWR_1) + sum(jul_aug_rs$CWR_2)
  }
}

