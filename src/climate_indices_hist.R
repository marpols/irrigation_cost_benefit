R1mm_hist <- calc_clim_index(climate_hist,
                index = "R1mm",
                by = c("stncode", "ian"),
                prcp_col = "Precipitation",
                start_mo = 5,
                end_mo = 9)
R1mm_hist$drydays <- 153 - R1mm_hist$R1mm

R10mm_hist <- calc_clim_index(climate_hist,
                             index = "R10mm",
                             by = c("stncode", "ian"),
                             prcp_col = "Precipitation",
                             start_mo = 5,
                             end_mo = 9)

R20mm_hist <- calc_clim_index(climate_hist,
                              index = "R20mm",
                              by = c("stncode", "ian"),
                              prcp_col = "Precipitation",
                              start_mo = 5,
                              end_mo = 9)

CWD_hist <- calc_clim_index(climate_hist,
                              index = "CWD",
                              by = c("stncode", "ian"),
                              prcp_col = "Precipitation",
                              start_mo = 5,
                              end_mo = 9)

CDD_hist <- calc_clim_index(climate_hist,
                              index = "CDD",
                              by = c("stncode", "ian"),
                              prcp_col = "Precipitation",
                              start_mo = 5,
                              end_mo = 9)

TX35_hist <- calc_clim_index(climate_hist,
                              index = "TX35",
                              by = c("stncode", "ian"),
                              tmax_col = "MaxTemp",
                              start_mo = 5,
                              end_mo = 9)

TX30_hist <- calc_clim_index(climate_hist,
                             index = "TX30",
                             by = c("stncode", "ian"),
                             tmax_col = "MaxTemp",
                             start_mo = 5,
                             end_mo = 9)

cihistl <- list(R1mm_hist, R10mm_hist, R20mm_hist, CWD_hist, CDD_hist, TX35_hist, TX30_hist)
ci_hist <- cihistl %>% 
  reduce(left_join, by = c("stncode", "ian", "year"))
