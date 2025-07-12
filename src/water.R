is.irrigated <- function(factor, threshold, compare){
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

#WB TEST--------------------------------------------------

calc.water.balance <- function(sims_data, hist_annual, hist_monthly,
                               stn, soil){
  
  year <- unique(sims_data$ian)
  
  #GS
  df <- sims_data |> 
    filter(mo %in% c(6:9)) |> 
    data.table::as.data.table()
  
  a <- df[mo == 6 & jo == 1,11:15] |> unlist()
  initial_water <- sum(a)
  b <- df[mo == 9 & jo == 30,11:15] |> unlist()
  final_water <-  sum(b)
  
  ETa <- (df[mo == 9 & jo == 30, 21] - df[mo == 6 & jo == 1, 21]) |> unlist()
  
  hist <- data.table::as.data.table(hist_annual)
  cum_precip <- hist[station == stn & ian == year & period == "GS", 4] |>
    unlist()
  
  cum_precip_e <- sum(df[,precip])
  
  water_loss <- cum_precip - cum_precip_e
  
  WB_gs <- (cum_precip + initial_water) - (final_water + ETa + water_loss)
  
  #monthly WB
  df_wb <- df |> group_by(mo) |>
    summarise(ETa = sum(et),
              cum_precip_e = sum(precip),
    )
  
  df_summary <- df |>
    # Step 1: Combine year, month, day into a proper Date column
    mutate(date = make_date(ian, mo, jo)) |>
    
    # Step 2: Add year-month group for monthly aggregation
    mutate(year_month = floor_date(date, "month")) |>
    
    # Step 3: Filter rows that are first or last date in each month
    group_by(year_month) |>
    filter(date == min(date) | date == max(date)) |>
    ungroup() |>
    
    # Step 4: Sum selected columns row-wise (adjust col1:col5 if needed)
    rowwise() |>
    mutate(SWC = sum(c_across(11:15), na.rm = TRUE)) |>
    
    # Step 5: Return relevant columns
    select(date, SWC)
  
  monthly <- data.table::as.data.table(hist_monthly)
  june <- monthly[mo == 6 & ian == year & station == stn, precip.cum]
  july <-monthly[mo == 7 & ian == year & station == stn, precip.cum]
  aug <-monthly[mo == 8 & ian == year & station == stn, precip.cum]
  sept <- monthly[mo == 9 & ian == year & station == stn, precip.cum]
  
  june_wl <- june - df_wb[[1, 3]]
  july_wl <- july - df_wb[[2, 3]] 
  aug_wl <- aug - df_wb[[3, 3]] 
  sept_wl <- sept - df_wb[[4, 3]] 
  
  WB_june <- (june + df_summary[[1,2]]) - (df_summary[[2,2]] + df_wb[[1,2]] + june_wl)
  WB_july <- (july + df_summary[[3,2]]) - (df_summary[[4,2]] + df_wb[[2,2]] + july_wl)
  WB_aug <- (aug + df_summary[[5,2]]) - (df_summary[[6,2]] + df_wb[[3,2]] + aug_wl)
  WB_sept <- (sept + df_summary[[7,2]]) - (df_summary[[8,2]] + df_wb[[4,2]] + sept_wl)
  
  gs_row <- c(soil, stn, year, "GS",
              initial_water, cum_precip,
              final_water, ETa, water_loss, WB_gs)
  
  june_row <- c(soil, stn, year, "6",
                df_summary[[1,2]], june,
                df_summary[[2,2]], df_wb[[1,2]], june_wl, WB_june)
  july_row <- c(soil, stn, year, "7",
                df_summary[[3,2]], july,
                df_summary[[4,2]], df_wb[[2,2]], july_wl, WB_july)
  aug_row <- c(soil, stn, year, "8",
               df_summary[[5,2]], aug,
               df_summary[[6,2]], df_wb[[3,2]], aug_wl, WB_aug)
  sept_row <- c(soil, stn, year, "9",
                df_summary[[7,2]], sept,
                df_summary[[8,2]], df_wb[[4,2]], sept_wl, WB_sept)
  
  wb_summary <- rbind(t(gs_row), t(june_row), t(july_row),
                      t(aug_row), t(sept_row)) |>
    as.data.frame()
  names(wb_summary) <- c("soil", "station", "year", "mo", "initial_water",
                         "precipitation", "final_water", "ETa", "water_loss",
                         "WB")
  return(wb_summary)
}

calc.wb <- function(sims_data){
  wb <- mapply(function(sim, name){
    str <- str_split(name, "_") |> unlist()
    stn <- str[1]
    soil <- str[2]
    stn_full <- get.stn(stn)
    lapply(sim, calc.water.balance, 
           hist_annual = yearly, 
           hist_monthly = monthly,
           stn = stn_full, 
           soil = soil)
  },
  sims_data[[1]], names(sims_data[[1]]),
  SIMPLIFY = FALSE)
}

 get.wb <- function(sims_data){
   water_balance <- calc.wb(sims_data)
   wb_all <- do.call(rbind, water_balance |> flatten())
   saveRDS(wb_all, "data/waterbalance_all.RDS")
 }
 
RAW.calculation <- function(sims_data, soil, stn){
   # https://www.fao.org/4/x0490e/x0490e0e.htm
   # soil_WPs <- c("CTW" = 0.07524, "ARY" = 0.07344, "CLO" = 0.0525)
   # WP <- soil_WPs[[soil]]
   # FC <- mean(sims_data[1, "HR_vol_1_30"], sims_data[1, "HR_vol_31_60"])
   # #Zr <- max(sims_data$zrac)
   # Zr <- 60 #max rooting depth
   
   root_zone_WC <- sims_data |> group_by(mo) |>
     summarise(layer1 = mean(HR_mm.1.),
               layer2 = mean(HR_mm.2.),
               layer3 = mean(HR_mm.3.)) |>
     data.table::as.data.table()
   
   root_zone_WC$total <- root_zone_WC$layer1 + root_zone_WC$layer2 + root_zone_WC$layer3
   
   root_zone_WC$RAW <- root_zone_WC$total * 0.35
   root_zone_WC$soil <- soil
   root_zone_WC$stn_code <- stn
   root_zone_WC$year <- unique(sims_data$ian)
   
   return(root_zone_WC)
 }
 
calc.RAW <- function(sims_data){
   raw <- mapply(function(sim, name){
     str <- str_split(name, "_") |> unlist()
     stn <- str[1]
     soil <- str[2]
     lapply(sim, RAW.calculation,
            stn = stn, 
            soil = soil)
   },
   sims_data[[1]], names(sims_data[[1]]),
   SIMPLIFY = FALSE)
 }
 
get.RAW <- function(sims_data){
   readily_available_water <- calc.RAW(sims_data)
   raw_all <- do.call(rbind, readily_available_water |> flatten())
   saveRDS(wb_all, "data/readily_available_water_all.RDS")
  
   return(raw_all)
 }


