syscosts <- irrcosts()
envcosts <- envcosts()
nyears_hist <- 20
nyears_proj <- length(unique(analysis_proj[["ian"]]))

# CBnet with externalities ------------------------------------------------

pbps_hist_ext <- list()
pbps_proj_ext <- list()
win <- as.data.frame(t(embed(2001:2024, nyears_hist)))

for(sys in syscosts){
  col <- sprintf("%s_SIC", sys[["id"]])
  CBcol70 <- sprintf("CByr_70_%s", sys[["id"]])
  CBcol90 <- sprintf("CByr_90_%s", sys[["id"]])
  
  lvls <- c("min", "mid", "max")
  
  lapply(lvls, function(l){
    CB70 <- sprintf("%s_%s", CBcol70, l)
    CB90 <- sprintf("%s_%s", CBcol90, l)
    
    analysis_hist[
      , c(CB70,CB90) := .(
        CB(gross_70, get(col), envext(dn2o,
                                      dnl,
                                      envcosts[lvl == l, climate_N2O],
                                      envcosts[lvl == l, health_N2O],
                                      envcosts[lvl == l, ecosystem_NO3],
                                      envcosts[lvl == l, health_NO3])),
        CB(gross_90, get(col), envext(dn2o,
                                      dnl,
                                      envcosts[lvl == l, climate_N2O],
                                      envcosts[lvl == l, health_N2O],
                                      envcosts[lvl == l, ecosystem_NO3],
                                      envcosts[lvl == l, health_NO3]))
      )
    ]
    
    analysis_proj[
      , c(CB70,CB90) := .(
        CB(gross_70, get(col), envext(dn2o,
                                      dnl,
                                      envcosts[lvl == l, climate_N2O],
                                      envcosts[lvl == l, health_N2O],
                                      envcosts[lvl == l, ecosystem_NO3],
                                      envcosts[lvl == l, health_NO3])),
        CB(gross_90, get(col), envext(dn2o,
                                      dnl,
                                      envcosts[lvl == l, climate_N2O],
                                      envcosts[lvl == l, health_N2O],
                                      envcosts[lvl == l, ecosystem_NO3],
                                      envcosts[lvl == l, health_NO3]))
      )
    ]
    
    lhist <- lapply(names(win), function(w){
      sub <- analysis_hist[ian %in% win[[w]]]
      list("70" = STICSdt::get_sum(as.data.frame(sub),
                                   by = c("stncode", "soilcode"),
                                   CB70),
           "90" = STICSdt::get_sum(as.data.frame(sub),
                                   by = c("stncode", "soilcode"),
                                   CB90)
      )
    }) |> purrr::list_flatten()
    
    lhist70 <- lhist[which(names(lhist) == "70")] |> 
      purrr::list_rbind() |>
      STICSdt::get_mean(var = paste0(CB70, "_sum"),
                        by = c("stncode", "soilcode")) |>
      dplyr::rename("{CB70}_sum" := paste0(CB70, "_sum_mean"))
    
    lhist90 <- lhist[which(names(lhist) == "90")] |> 
      purrr::list_rbind() |>
      STICSdt::get_mean(var = paste0(CB90, "_sum"),
                        by = c("stncode", "soilcode")) |>
      dplyr::rename("{CB90}_sum" := paste0(CB90, "_sum_mean"))
    
    lhist <- list("70" = data.table::as.data.table(lhist70),
                  "90" = data.table::as.data.table(lhist90))
    
    pbps_hist_ext[[sys[["type"]]]][[l]] <<- lhist
    
    lproj <- list("70" = STICSdt::get_sum(as.data.frame(analysis_proj),
                                          by = c("model", "ssp", "soilcode"),
                                          CB70),
                  "90" = STICSdt::get_sum(as.data.frame(analysis_proj),
                                          by = c("model", "ssp", "soilcode"),
                                          CB90)
    )
    
    pbps_proj_ext[[sys[["type"]]]][[l]] <<- lproj
    
  })
  
}


# Payback periods with externalities --------------------------------------

PP_hist_ext <- lapply(names(pbps_hist_ext), function(sys){
  
  info <- syscosts[[sys]]
  
  lapply(lvls, function(l){
    
    df <- STICSdt::join_dfs(list(pbps_hist_ext[[sys]][[l]][["70"]],
                                 pbps_hist_ext[[sys]][[l]][["90"]]),
                            by = c("stncode", "soilcode")) |>
      data.table::as.data.table()
    
    PPcol70 <- sprintf("PP70_ext_%s", l)
    PPcol90 <- sprintf("PP90_ext_%s", l)
    CB70 <- sprintf("CByr_70_%s_%s_sum", info[["id"]], l)
    CB90 <- sprintf("CByr_90_%s_%s_sum", info[["id"]], l)
    CBnet70 <- sprintf("CBnet70_%s", l)
    CBnet90 <- sprintf("CBnet90_%s", l)
    
    df[, c("system", PPcol70, PPcol90, CBnet70, CBnet90, CB70, CB90):= .(
      sys,
      PP(info[["total.asset"]], get(CB70), nyears_hist),
      PP(info[["total.asset"]], get(CB90), nyears_hist),
      get(CB70),
      get(CB90),
      NULL,
      NULL
    )]
  }) |> (\(tables) Reduce(
    \(x, y) merge(
      x, y,
      by = c("stncode", "soilcode", "system"),
      all = TRUE
    ),
    tables
  ))()

}) |> purrr::list_rbind() |> data.table::as.data.table()

PP_proj_ext <- lapply(names(pbps_proj_ext), function(sys){
  
  info <- syscosts[[sys]]
  
  lapply(lvls, function(l){
    
    df <- STICSdt::join_dfs(list(pbps_proj_ext[[sys]][[l]][["70"]],
                                 pbps_proj_ext[[sys]][[l]][["90"]]),
                            by = c("model", "ssp", "soilcode")) |>
      data.table::as.data.table()
    
    PPcol70 <- sprintf("PP70_ext_%s", l)
    PPcol90 <- sprintf("PP90_ext_%s", l)
    CB70 <- sprintf("CByr_70_%s_%s_sum", info[["id"]], l)
    CB90 <- sprintf("CByr_90_%s_%s_sum", info[["id"]], l)
    CBnet70 <- sprintf("CBnet70_%s", l)
    CBnet90 <- sprintf("CBnet90_%s", l)
    
    df[, c("system", PPcol70, PPcol90, CBnet70, CBnet90, CB70, CB90):= .(
      sys,
      PP(info[["total.asset"]], get(CB70), nyears_hist),
      PP(info[["total.asset"]], get(CB90), nyears_hist),
      get(CB70),
      get(CB90),
      NULL,
      NULL
    )]
  }) |> (\(tables) Reduce(
    \(x, y) merge(
      x, y,
      by = c("model", "ssp", "soilcode", "system"),
      all = TRUE
    ),
    tables
  ))()

}) |> purrr::list_flatten() |> purrr::list_rbind()

PP_proj_ext_means <- STICSdt::summarise_vars(PP_proj_ext,
                                         var = grep("PP", names(PP_proj_ext), value = TRUE),
                                         type = "mean",
                                         by = c("ssp", "soilcode", "system")) |>
  data.table::as.data.table()

PP_hist[, c("thrsh70", "thrsh90") := .(
  PP70 < 20,
  PP90 < 20)]

PP_proj_means[, c("thrsh70", "thrsh90") := .(
  PP70_mean < 20,
  PP90_mean < 20)]
