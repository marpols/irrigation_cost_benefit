get_datasmry <- function(df_list,
                         dt_list,
                         by,
                         names = c("fixed", "dynamic")){
  mapply(function(i,j){
    max_vals <- lapply(i, function(df){
      STICSdt::summarise_vars(df, 
                              c("n_tot_irrigations",
                                "totir",
                                "pdsfruitfrais",
                                "Qles",
                                "Qem_N2O",
                                "QNplante"), 
                              type = "max", 
                              by = by)
    }) |> setNames(c("noirr", "autoirr"))
    
    
    j[, `:=`(
      numirr = max_vals[["autoirr"]][["n_tot_irrigations_max"]],
      totir  = max_vals[["autoirr"]][["totir_max"]]
    )]
    
    j[, c("yield", "yield_irr") := .(
      max_vals[["noirr"]][["pdsfruitfrais_max"]],
      max_vals[["autoirr"]][["pdsfruitfrais_max"]]
    )][
      , `:=`(
        yg = yield_irr - yield
      )
    ][
      yg <= 0, c("yg","numirr") := 0
    ]
    
    j[, c("leaching", "leaching_irr") := .(
      max_vals[["noirr"]][["Qles_max"]],
      max_vals[["autoirr"]][["Qles_max"]]
    )][
      , `:=`(
        dnl = leaching_irr - leaching
      )
    ][
      yg <= 0, dnl := 0
    ]
    
    j[, c("N2O", "N2O_irr") := .(
      max_vals[["noirr"]][["Qem_N2O_max"]],
      max_vals[["autoirr"]][["Qem_N2O_max"]]
    )][
      , `:=`(
        dn2o = N2O_irr - N2O
      )
    ][
      yg <= 0, dn2o := 0
    ]
    
    j[, c("plntN", "plntN_irr") := .(
      max_vals[["noirr"]][["QNplante_max"]],
      max_vals[["autoirr"]][["QNplante_max"]]
    )][
      , `:=`(
        dplntN = plntN_irr - plntN
      )
    ][
      yg <= 0, dplntN := 0
    ]
    
    list(analysis = j, max = max_vals)
    
  }, df_list,
  dt_list,
  SIMPLIFY = FALSE) |> 
    setNames(names)
}


