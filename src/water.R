irrigated <- function(factor, threshold, compare){
  if(compare == "lt") return(factor < threshold) else return(factor > threshold)
}

calc.water.requirements <- function(sim, 
                                    method = "PET",
                                    start = c(6,1),
                                    end = c(9,30)){
  
  gs <- sim[which(sim$mo == start[1] & sim$jo == start[2]):
              which(sim$mo == end[1] & sim$jo == end[2]),]
  
  precip.PET <- function() {
    gs_et0 <- sum(gs$et0)
    precip_e <- sum(gs$precip)
    gs_cetm <- max(gs$cetm) - min(gs$cetm)
    
    CWR_gs <- gs_cetm - precip_e
    
    
    jul_aug <- sim[which(sim$mo == 7 & sim$jo == 1):which(sim$mo == 8 &
                                                            sim$jo == 30), ]
    
    jul_aug_cetm <- max(jul_aug$cetm) - min(jul_aug$cetm)
    jul_aug_eff_precip <- sum(jul_aug$precip)
    
    CWR_jul_aug <- sum(jul_aug_cetm - jul_aug_eff_precip)
    
    return(data.frame(rbind(
      c(
        cetm = as.double(gs_cetm),
        precip_e = as.double(precip_e),
        CWR = as.double(CWR_gs),
        period = "GS"
      ),
      c(
        cetm = as.double(jul_aug_cetm),
        precip_e = as.double(jul_aug_eff_precip),
        CWR = as.double(CWR_jul_aug),
        period = "julaug"
      )
    )))
    
    # return(list(
    #   "Growing Season" = list(
    #     cetm = gs_cetm,
    #     precip = precip_e,
    #     CWR = CWR_gs
    #   ),
    #   "July August" = list(
    #     cetm = jul_aug_cetm,
    #     precip = jul_aug_eff_precip,
    #     CWR = CWR_jul_aug
    #   )
    # ))
    
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
    
    return(list(
      "method 1" = list(
      "Planting to Harvest" = CWR_gs_rs_v1,
      "July August" = CWR_jul_aug_rs_v1),
      "method 2" = list(
        "Planting to Harvest" = CWR_gs_rs_v2,
        "July August" = CWR_jul_aug_rs_v2)
    ))
  }
  
  if(method == "PET"){
   return(precip.PET())
  } else {
    return(root.zone())
  }
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


