climate_hist <- STICSdt::get_climate_files(js_path = javastics_path,
                                           ws = "irrigation_assessment",
                                           return_type = 1) |>
  filter(stncode %in% c("EP", "HC", "NG", "SUMMERSIDE"), mo %in% seq(5,9))
climate_hist[stncode == "SUMMERSIDE", stncode := "S"][
  stncode == "HC", stncode := "HCC"
]

climate_proj <- STICSdt::get_climate_files(js_path = javastics_path,
                                           name_format = "model_ssp_location",
                                           ws = "irrigation_assessment_projections",
                                           return_type = 1) |>
  filter(mo %in% seq(5,9))


# Historical --------------------------------------------------------------

stnfac <- c("SUMMERSIDE", "NG", "HC", "EP")
STICSdt::plot_boxplot(STICSdt::get_sum(climate_hist, "Precipitation",
                                       by = c("stncode", "ian")),
                      factors = stnfac,
                      "Precipitation_sum")
STICSdt::plot_boxplot(STICSdt::get_mean(climate_hist, "SolarRad",
                                       by = c("stncode", "ian")),
                      factors = stnfac,
                      "SolarRad_mean")
STICSdt::plot_boxplot(STICSdt::get_mean(climate_hist, "WindSpeed",
                                        by = c("stncode", "ian")),
                      factors = stnfac,
                      "WindSpeed_mean")
# STICSdt::plot_boxplot(STICSdt::get_gdd(climate_hist, "SolarRad",
#                                         by = c("stncode", "ian")),
#                       factors = stnfac,
#                       "SolarRad_mean")

STICSdt::plot_variable(STICSdt::get_sum(climate_hist, "Precipitation",
                                        by = c("stncode", "ian")),
                       "Precipitation_sum",
                       by = "stncode")

# Projections -------------------------------------------------------------


sspfac <- c("ssp126", "ssp370", "ssp585")
STICSdt::plot_boxplot(STICSdt::get_sum_mean(climate_proj, "Precipitation",
                                       mean_by = c("ssp", "ian", "jo"),
                                       sum_by = c("ssp", "ian")),
                      x_axis = "ssp",
                      factors = sspfac,
                      "Precipitation_mean_sum")
STICSdt::plot_boxplot(STICSdt::get_mean(climate_proj, "SolarRad",
                                            by = c("ssp", "ian", "jo")),
                      x_axis = "ssp",
                      factors = sspfac,
                      "SolarRad_mean")
STICSdt::plot_boxplot(STICSdt::get_mean(climate_proj, "WindSpeed",
                                        by = c("ssp", "ian", "jo")),
                      x_axis = "ssp",
                      factors = sspfac,
                      "WindSpeed_mean")
