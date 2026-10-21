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

climhist_summarygs <- STICSdt::get_clim_sum(climate_hist, mo_range = 5:9)
climproj_summarygs <- STICSdt::get_clim_sum(climate_proj,
                                          mo_range = 5:9,
                                          by = c("model", "ssp", "ian"),
                                          fname_format = "model_ssp_location")
climREF_sumgs <- STICSdt::get_clim_sum(climate_REF,
                                     mo_range = 5:9,
                                     by = c("model", "ssp", "ian"),
                                     fname_format = "model_ssp_location")


# SPEI --------------------------------------------------------------------

lats_hist <- get_lats(climate_hist) |> 
  STICSdt::as.climate() |> 
  STICSdt::add_ids("stn name")
lats_proj <- data.frame(model = unique(climate_proj$model), latitude = 46.3)


hist_SPEI <- calc_SPEI(climate_hist,
                       unique(climate_hist$stncode),
                       latitudes = lats_hist)

hist_SPEI2 <- calc_SPEI(climate_hist,
                       unique(climate_hist$stncode),
                       etfor = "STICS",
                       sims = hist_dynamic$noirr,
                       latitudes = lats_hist,
                       joinby = "stncode")


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

#number of years per spei_class
nyrs_hist <- spei_sum(hist_SPEI)
nyrs_hist2 <- spei_sum(hist_SPEI2,
                       var = "stncode",
                       by = c("stncode", "soilcode"))

nyrs_proj <- spei_sum(proj_SPEI,
                      var = "model",
                      by = c("model", "ssp"))
nyrs_REF <- spei_sum(REF_SPEI,
                     var = "model",
                     by = c("model", "ssp"))

tproj <- t(nyrs_proj[,-1]) |> 
  data.table::as.data.table() |> 
  purrr::set_names(nyrs_proj[[1]])
tnyrs_proj <- tproj[, file_name := names(nyrs_proj)[-1]] |>
  STICSdt::add_ids("model_ssp") 
tnyrs_projavg <- tnyrs_proj |>
  dplyr::slice(1:(dplyr::n()-2)) |>
  dplyr::ungroup() |>
  dplyr::summarise(across(where(is.double), ~mean(as.numeric(.x))),.by = "ssp")



for(pds in c("fixed", "dynamic")){
  data_hist[[pds]]$analysis[hist_SPEI[MONTH == 8,ian := YEAR],
                           spei_class := i.spei_class, on = .(stncode, ian)]
  data_proj[[pds]]$analysis[proj_SPEI[MONTH == 8,ian := YEAR],
                           spei_class := i.spei_class, on = .(model, ssp, ian)]
}

data_hist2 <- lapply(list("fixed", "dynamic"), \(pds){
  data_hist[[pds]]$analysis[hist_SPEI2[MONTH == 8,ian := YEAR],
                                               spei_class := i.spei_class,
                            on = .(stncode, ian)]
}) |> purrr::set_names("fixed", "dynamic")


data_REF[REF_SPEI[MONTH == 8,ian := YEAR],
         spei_class := i.spei_class, on = .(model, ssp, ian)]

# Climate Indices ---------------------------------------------------------

climind_hist <- get_climindx(climate_hist)
climind_hist_mean <- get_climindx(climate_hist,
                                  means = TRUE,
                                  meanby = c("ian")) 
climind_proj <- get_climindx(climate_proj,
                             by = c("model", "ssp", "ian"),
                             means = FALSE)
climind_proj_mean <- get_climindx(climate_proj,
                             by = c("model", "ssp", "ian"),
                             means = TRUE)
climind_REF <- get_climindx(climate_REF,
                            by = c("model", "ssp", "ian"),
                            means = FALSE)
climind_REF_mean <- get_climindx(climate_REF,
                            by = c("model", "ssp", "ian"),
                            means = TRUE)
