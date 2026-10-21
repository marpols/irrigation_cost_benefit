histtbl <- lapply(list("fixed", "dynamic"), \(exp){
  
  CByrcols <- grep("CByr", names(data_hist[[exp]]$analysis), value = TRUE)
  
  data_hist[[exp]]$analysis |> dplyr::summarise(dplyr::across(all_of(c("yg",
                                                      "totir",
                                                      "dnl",
                                                      "dn2o",
                                                      "dplntN",
                                                      "swfac2moy_noirr",
                                                      "turfac2moy_noirr",
                                                      CByrcols)),
                                             mean, na.rm = TRUE),
                               .by = c("stncode", "soilcode", "spei_class"))
  
}) |> purrr::set_names(c("fixed", "dynamic"))

histtbl2 <- lapply(list("fixed", "dynamic"), \(exp){
  
  CByrcols <- grep("CByr", names(data_hist2[[exp]]), value = TRUE)
  
  data_hist2[[exp]] |> dplyr::summarise(dplyr::across(all_of(c("yg",
                                                      "totir",
                                                      "dnl",
                                                      "dn2o",
                                                      "dplntN",
                                                      "swfac2moy_noirr",
                                                      "turfac2moy_noirr",
                                                      CByrcols)),
                                             mean, na.rm = TRUE),
                               .by = c("stncode", "soilcode", "spei_class"))
  
}) |> purrr::set_names(c("fixed", "dynamic"))

projtbl <- lapply(list("fixed", "dynamic"), \(exp){
  # proj_all <- dplyr::inner_join(data_proj[[exp]]$analysis, 
  #                               proj_SPEI[MONTH == 8][,ian := YEAR][
  #                                 , .(model,ssp,ian,spei_class)
  #                               ],
  #                               by = c("model", "ssp", "ian"))
  
  CByrcols <- grep("CByr", names(proj_all), value = TRUE)
  
  data_proj[[exp]]$analysis |> dplyr::summarise(dplyr::across(all_of(c("yg",
                                                      "totir",
                                                      "dnl",
                                                      "dn2o",
                                                      "dplntN",
                                                      CByrcols)),
                                             mean, na.rm = TRUE),
                               .by = c("ssp", "soilcode", "spei_class"))
  
}) |> purrr::set_names(c("fixed", "dynamic"))
