hist_all <- inner_join(analysis_hist, 
                       hist_SPEI[MONTH == 8][,ian := YEAR][
                         , .(file_name, stncode,ian,class)
                       ],
                       by = c("stncode", "ian"))


CByrcols <- grep("CByr", names(hist_all), value = TRUE)
SPEImeans_hist <- hist_all |> summarise(across(c("yg", CByrcols),
                                               mean, na.rm = TRUE),
                                        .by = c("stncode", "soilcode", "class"))

proj_all <- inner_join(analysis_proj, 
                       hist_SPEI[MONTH == 8][,ian := YEAR][
                         , .(model,ssp,ian,class)
                       ],
                       by = c("model", "ssp", "ian"))


CByrcols <- grep("CByr", names(proj_all), value = TRUE)
SPEImeans_proj <- proj_all |> summarise(across(c("yg", CByrcols), mean, na.rm = TRUE),
                                        .by = c("ssp", "soilcode", "class"))