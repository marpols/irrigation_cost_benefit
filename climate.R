climate_hist <- STICSdt::get_climate_files(ws = "irrigation_assessment",
                                       return_type = 1)[stncode %in% c("EP", "HCC", "NG", "S"),]

climate_proj <- STICSdt::get_climate_files(name_format = "model_ssp_location",
                                       ws = "irrigation_assessment_projections",
                                                       return_type = 1)

climate_REF <- climate_proj[ian %in% 2001:2024,]

climate_proj <- climate_proj[ian %in% 2027:2047,]

climhist_summary <- STICSdt::get_clim_sum(climate_hist)
climproj_summary <- STICSdt::get_clim_sum(climate_proj,
                                          by = c("model", "ssp", "ian"),
                                          fname_format = "model_ssp_location")
climREF_sum <- STICSdt::get_clim_sum(climate_REF,
                                     by = c("model", "ssp", "ian"),
                                     fname_format = "model_ssp_location")


# SPEI --------------------------------------------------------------------

lats_hist <- get_lats(climate_hist)
lats_proj <- data.frame(model = unique(climate_proj$model), latitude = 46.3)

hist_SPEI <- calc_SPEI(climate_hist,
                       unique(climate_hist$file_name),
                       latitudes = lats_hist)


proj_SPEI <- calc_SPEI(climate_proj,
                       unique(climate_proj$model),
                       latitudes = lats_proj,
                       by = c("model", "ssp", "ian", "mo"),
                       joinby = "model")

REF_SPEI <- calc_SPEI(climate_REF,
                      unique(climate_REF$model),
                      latitudes = lats_proj,
                      by = c("model", "ssp", "ian", "mo"),
                      joinby = "model")

nyrs_hist <- spei_sum(hist_SPEI)
nyrs_proj <- spei_sum(proj_SPEI,
                      var = "model",
                      by = c("model", "ssp"))
nyrs_REF <- spei_sum(REF_SPEI,
                     var = "model",
                     by = c("model", "ssp"))


