get_datasmry <- function(df_list,
                         dt_list,
                         by,
                         names = c("fixed", "dynamic")) {
  mapply(function(i, j) {
    max_vals <- lapply(i, function(df) {
      STICSdt::summarise_vars(
        df,
        c(
          "n_tot_irrigations",
          "totir",
          "pdsfruitfrais",
          "Qles",
          "Qem_N2O",
          "QNplante"
        ),
        type = "max",
        by = by
      )
    }) |> setNames(c("noirr", "autoirr"))
    
    
    mean_stress <- lapply(i, function(df) {
      x <- df |>
        dplyr::summarise(
          swfac_noirr = mean(swfac),
          turfac_noirr = mean(turfac),
          swfac2moy_noirr = mean(swfac2moy),
          turfac2moy_noirr = mean(turfac2moy),
          .by = all_of(by)
        )
    }) |> setNames(c("noirr", "autoirr"))
    
    min_stress <- lapply(i, function(df) {
      x <- df |>
        dplyr::summarise(
          swfac_min = min(swfac),
          turfac_min = min(turfac),
          swfac2moy_min = min(swfac2moy),
          turfac2moy_min = min(turfac2moy),
          .by = all_of(by)
        )
    }) |> setNames(c("noirr", "autoirr"))
    
    i$noirr <- data.table::as.data.table(i$noirr)
    
    j[, 
      `:=`(numirr = max_vals[["autoirr"]][["n_tot_irrigations_max"]],
             totir  = max_vals[["autoirr"]][["totir_max"]])]
    
    j[,
      c("yield", "yield_irr") := 
        .(max_vals[["noirr"]][["pdsfruitfrais_max"]],
          max_vals[["autoirr"]][["pdsfruitfrais_max"]])][,
                                                         `:=`(yg = yield_irr - yield)][
                                                           yg <= 0, c("yg", "numirr", "totir") := 0]
    
    j[, 
      c("leaching", "leaching_irr") := 
        .(max_vals[["noirr"]][["Qles_max"]], 
          max_vals[["autoirr"]][["Qles_max"]])][,
                                                `:=`(dnl = leaching_irr - leaching)][
                                                  yg <= 0, dnl := 0]
    
    j[, 
      c("N2O", "N2O_irr") := 
        .(max_vals[["noirr"]][["Qem_N2O_max"]],
          max_vals[["autoirr"]][["Qem_N2O_max"]])][,
                                                   `:=`(dn2o = N2O_irr - N2O)][
                                                     yg <= 0, dn2o := 0]
    
    j[, c("plntN", "plntN_irr") :=
        .(max_vals[["noirr"]][["QNplante_max"]],
          max_vals[["autoirr"]][["QNplante_max"]])][,
                                                    `:=`(dplntN = plntN_irr - plntN)][
                                                      yg <= 0, dplntN := 0]
    
    j[, c("swfac_noirr",
        "swfac2moy_noirr",
        "turfac_noirr",
        "turfac2moy_noirr") := 
        .(mean_stress$noirr[["swfac_noirr"]],
          mean_stress$noirr[["swfac2moy_noirr"]],
          mean_stress$noirr[["turfac_noirr"]],
          mean_stress$noirr[["turfac2moy_noirr"]])]
    
    j[, c("swfac_min",
        "swfac2moy_min",
        "turfac_min",
        "turfac2moy_min") := 
        .(min_stress$noirr[["swfac_min"]],
          min_stress$noirr[["swfac2moy_min"]],
          min_stress$noirr[["turfac_min"]],
          min_stress$noirr[["turfac2moy_min"]])]
    
    list(analysis = j, max = max_vals)
    
  }, df_list, dt_list, SIMPLIFY = FALSE) |>
    setNames(names)
}
