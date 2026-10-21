get_climindx <- function(dt,
                         by = c("stncode", "ian"),
                         start_mo = 5,
                         end_mo = 9,
                         means = FALSE,
                         meanby = c("ssp", "ian")) {
  
  daytot <- lapply(start_mo:end_mo, lubridate::days_in_month) |> unlist() |> sum()
  
  R1mm <- calc_clim_index(
    dt,
    index = "R1mm",
    by = by,
    prcp_col = "Precipitation",
    start_mo = start_mo,
    end_mo = end_mo
  )
  R1mm$drydays <- daytot - R1mm$R1mm
  
  R10mm <- calc_clim_index(
    dt,
    index = "R10mm",
    by = by,
    prcp_col = "Precipitation",
    start_mo = start_mo,
    end_mo = end_mo
  )
  
  
  R20mm <- calc_clim_index(
    dt,
    index = "R20mm",
    by = by,
    prcp_col = "Precipitation",
    start_mo = start_mo,
    end_mo = end_mo
  )
  
  
  CWD <- calc_clim_index(
    dt,
    index = "CWD",
    by = by,
    prcp_col = "Precipitation",
    start_mo = start_mo,
    end_mo = end_mo
  )
  
  
  CDD <- calc_clim_index(
    dt,
    index = "CDD",
    by = by,
    prcp_col = "Precipitation",
    start_mo = start_mo,
    end_mo = end_mo
  )
  
  
  TX35 <- calc_clim_index(
    dt,
    index = "TX35",
    by = by,
    tmax_col = "MaxTemp",
    start_mo = start_mo,
    end_mo = end_mo
  )
  
  
  TX30 <- calc_clim_index(
    dt,
    index = "TX30",
    by = by,
    tmax_col = "MaxTemp",
    start_mo = start_mo,
    end_mo = end_mo
  )
  
  
  if(means){
    R1mm_mean <- STICSdt::summarise_vars(R1mm,
                                              c("R1mm", "drydays"),
                                              type = "mean",
                                              by = meanby)
    R10mm_mean <- STICSdt::get_mean(R10mm, "R10mm", by = meanby)
    R20mm_mean <- STICSdt::get_mean(R20mm, "R20mm", by = meanby)
    CWD_mean <- STICSdt::get_mean(CWD, "CWD_max", by = meanby)
    CDD_mean <- STICSdt::get_mean(CDD, "CDD_max", by = meanby)
    TX35_mean <- STICSdt::get_mean(TX35, "TX35", by = meanby)
    TX30_mean <- STICSdt::get_mean(TX30, "TX30", by = meanby)
    
    cil <- list(
      R1mm_mean,
      R10mm_mean,
      R20mm_mean,
      CWD_mean,
      CDD_mean,
      TX35_mean,
      TX30_mean
    )
    joinby <- meanby
    
  } else {
    cil <- list(
      R1mm,
      R10mm,
      R20mm,
      CWD,
      CDD,
      TX35,
      TX30
    )
    
    joinby <- c(by,"year")
  }
  

  cil |>
    purrr::reduce(dplyr::left_join, by = joinby) |>
    data.table::as.data.table()
}
