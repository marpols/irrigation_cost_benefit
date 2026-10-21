
javastics_path <- "C:\\Users\\marpo\\Documents\\Research\\STICS\\JavaSTICS-1.5.3-STICS-10.3.0"
usms_hist <- SticsRFiles::get_usms_list(file = file.path(javastics_path,
                                                    "irrigation_assessment",
                                                    "usms.xml")) |>
  grep("hills$", x = _, value= TRUE)

histby <- c("ian", "soilcode", "stncode")
projby <- c("ian", "model", "ssp", "soilcode")

hist_fixed <- lapply(list("historic_fixed_noirr",
                      "historic_fixed_autoirr"), \(x)
                    STICSdt::get_mod_s(
                      exp_dir = x,
                      exp_name_format = "period_planting_expid",
                      usm_name_format = "location_year_stncode_soilcode_ver",
                      ws = "irrigation_assessment",
                      return_type = 1
                    )) |> purrr::set_names("noirr", "autoirr")

hist_dynamic <- lapply(list("historic_dynamic_noirr-pet",
                            "historic_dynamic_autoirr"), \(x)
                       STICSdt::get_mod_s(
                         exp_dir = x,
                         exp_name_format = "period_planting_expid",
                         usm_name_format = "location_year_stncode_soilcode_ver",
                         ws = "irrigation_assessment",
                         return_type = 1
                       )) |> purrr::set_names("noirr", "autoirr")

proj_fixed <- lapply(list("projected_fixed_noirr",
                         "projected_fixed_autoirr"), \(x)
                    STICSdt::get_mod_s(
                      exp_dir = x,
                      exp_name_format = "period_planting_expid",
                      usm_name_format = "model_ssp_year_stncode_soilcode_ver",
                      ws = "irrigation_assessment_projections",
                      return_type = 1
                    )) |> purrr::set_names("noirr", "autoirr")

proj_dynamic <- lapply(list("projected_dynamic_noirr",
                            "projected_dynamic_autoirr"), \(x)
                       STICSdt::get_mod_s(
                         exp_dir = x,
                         exp_name_format = "period_planting_expid",
                         usm_name_format = "model_ssp_year_stncode_soilcode_ver",
                         ws = "irrigation_assessment_projections",
                         return_type = 1
                       )) |> purrr::set_names("noirr", "autoirr")

REF <- STICSdt::get_mod_s(
  exp_dir = "REF_fixed_noirr",
  exp_name_format = "period_planting_expid",
  usm_name_format = "model_ssp_year_stncode_soilcode_ver",
  ws = "irrigation_assessment_projections",
  return_type = 1
) |> data.table::as.data.table()
REF <- REF[ian %in% 2001:2024,]

analysis_histfx <- hist_fixed[[1]] |>
  dplyr::distinct(stncode, soilcode, ian, file_name) |>
  data.table::as.data.table()

analysis_histdyn <- hist_dynamic[[1]] |>
  dplyr::distinct(stncode, soilcode, ian, file_name) |>
  data.table::as.data.table()

analysis_projfx <- proj_fixed[[1]]  |>
  dplyr::distinct(model, ssp, soilcode, ian, file_name) |>
  data.table::as.data.table()

analysis_projdyn <- proj_dynamic[[1]]  |>
  dplyr::distinct(model, ssp, soilcode, ian, file_name) |>
  data.table::as.data.table()

analysis_REF <- REF |>
  dplyr::distinct(model, ssp, soilcode, ian, file_name) |>
  data.table::as.data.table()


data_hist <- get_datasmry(list(hist_fixed, hist_dynamic),
                          list(analysis_histfx, analysis_histdyn),
                          by= histby)

data_proj <- get_datasmry(list(proj_fixed, proj_dynamic),
                          list(analysis_projfx, analysis_projdyn),
                          by= projby)

data_REF <- STICSdt::summarise_vars(REF, 
                                    c("n_tot_irrigations",
                                      "totir",
                                      "pdsfruitfrais",
                                      "Qles",
                                      "Qem_N2O",
                                      "QNplante"), 
                                    type = "max", 
                                    by = projby) |> 
  data.table::as.data.table()

data_REF[, c("swfac2moy_noirr","turfac2moy_noirr") := .(
  REF[jul == 325, swfac2moy],
  REF[jul == 325, turfac2moy]
)]

