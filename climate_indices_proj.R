R1mm_proj <- calc_clim_index(climate_proj,
                             index = "R1mm",
                             by = c("model", "ssp", "ian"),
                             prcp_col = "Precipitation",
                             start_mo = 5,
                             end_mo = 9)
R1mm_proj$drydays <- 153 - R1mm_proj$R1mm
R1mm_proj_mean <- STICSdt::summarise_vars(R1mm_proj,
                                          c("R1mm", "drydays"),
                                          type = "mean",
                                          by = c("ssp", "ian"))

R10mm_proj <- calc_clim_index(climate_proj,
                              index = "R10mm",
                              by = c("model","ssp", "ian"),
                              prcp_col = "Precipitation",
                              start_mo = 5,
                              end_mo = 9)
R10mm_proj_mean <- STICSdt::get_mean(R10mm_proj,
                                     "R10mm",
                                     by = c("ssp", "ian"))

R20mm_proj <- calc_clim_index(climate_proj,
                              index = "R20mm",
                              by = c("model","ssp", "ian"),
                              prcp_col = "Precipitation",
                              start_mo = 5,
                              end_mo = 9)
R20mm_proj_mean <- STICSdt::get_mean(R20mm_proj,
                                     "R20mm",
                                     by = c("ssp", "ian"))

CWD_proj <- calc_clim_index(climate_proj,
                            index = "CWD",
                            by = c("model","ssp", "ian"),
                            prcp_col = "Precipitation",
                            start_mo = 5,
                            end_mo = 9)
CWD_proj_mean <- STICSdt::get_mean(CWD_proj,
                                     "CWD_max",
                                     by = c("ssp", "ian"))

CDD_proj <- calc_clim_index(climate_proj,
                            index = "CDD",
                            by = c("model","ssp", "ian"),
                            prcp_col = "Precipitation",
                            start_mo = 5,
                            end_mo = 9)
CDD_proj_mean <- STICSdt::get_mean(CDD_proj,
                                   "CDD_max",
                                   by = c("ssp", "ian"))

TX35_proj <- calc_clim_index(climate_proj,
                             index = "TX35",
                             by = c("model","ssp", "ian"),
                             tmax_col = "MaxTemp",
                             start_mo = 5,
                             end_mo = 9)
TX35_proj_mean <- STICSdt::get_mean(TX35_proj,
                                   "TX35",
                                   by = c("ssp", "ian"))

TX30_proj <- calc_clim_index(climate_proj,
                             index = "TX30",
                             by = c("model","ssp", "ian"),
                             tmax_col = "MaxTemp",
                             start_mo = 5,
                             end_mo = 9)
TX30_proj_mean <- STICSdt::get_mean(TX30_proj,
                                    "TX30",
                                    by = c("ssp", "ian"))

ciprojl <- list(R1mm_proj_mean, R10mm_proj_mean, R20mm_proj_mean, CWD_proj_mean,
                CDD_proj_mean, TX35_proj_mean, TX30_proj_mean)
ci_proj <- ciprojl %>% 
  reduce(left_join, by = c("ssp", "ian"))
