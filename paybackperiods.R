
results <- lapply(
  list(hist = data_hist, proj = data_proj),
  function(x) {
    lapply(x[c("fixed", "dynamic")], function(sublist) {
      yearly_SIC(sublist$analysis) |>
      yearly_gross()
      
    })
  }
)


results_hist <- lapply(results$hist, \(df) CBnet(df, 20) |> calc_pbps())
results_proj <- lapply(results$proj, \(df) CBnet(df, 20,
                                                 by = c("model", "ssp", "soilcode"))|>
                         calc_pbps(by = c("model", "ssp", "soilcode"),
                                   means = TRUE))


# CBnet -------------------------------------------------------------------
pbps_hist <- list()
pbps_proj <- list()

for(sys in syscosts){
  col <- sprintf("%s_SIC", sys[["id"]])
  CBcol70 <- sprintf("CByr_70_%s", sys[["id"]])
  CBcol90 <- sprintf("CByr_90_%s", sys[["id"]])
  
  analysis_hist[
    , c(CBcol70, CBcol90) := .(
      CB(gross_70, get(col)),
      CB(gross_90, get(col))
    )
  ]
  
  analysis_proj[
    , c(CBcol70, CBcol90) := .(
      CB(gross_70, get(col)),
      CB(gross_90, get(col))
    )
  ]
  
  win <- as.data.frame(t(embed(2001:2024, 20)))
  
  lhist <- lapply(names(win), function(w){
    sub <- analysis_hist[ian %in% win[[w]]]
    list("70" = STICSdt::get_sum(as.data.frame(sub),
                                 by = c("stncode", "soilcode"),
                                 CBcol70),
         "90" = STICSdt::get_sum(as.data.frame(sub),
                                 by = c("stncode", "soilcode"),
                                 CBcol90)
    )
  }) |> purrr::list_flatten()
  
  lhist70 <- lhist[which(names(lhist) == "70")] |> 
    purrr::list_rbind() |>
    STICSdt::get_mean(var = paste0(CBcol70, "_sum"),
                      by = c("stncode", "soilcode")) |>
    dplyr::rename("{CBcol70}_sum" := paste0(CBcol70, "_sum_mean"))
  
  lhist90 <- lhist[which(names(lhist) == "90")] |> 
    purrr::list_rbind() |>
    STICSdt::get_mean(var = paste0(CBcol90, "_sum"),
                      by = c("stncode", "soilcode")) |>
    dplyr::rename("{CBcol90}_sum" := paste0(CBcol90, "_sum_mean"))
  
  lhist <- list("70" = data.table::as.data.table(lhist70),
                "90" = data.table::as.data.table(lhist90))
  
  pbps_hist[[sys[["type"]]]] <- lhist
  
  lproj <- list("70" = STICSdt::get_sum(as.data.frame(analysis_proj),
                                        by = c("model", "ssp", "soilcode"),
                                        CBcol70),
                "90" = STICSdt::get_sum(as.data.frame(analysis_proj),
                                        by = c("model", "ssp", "soilcode"),
                                        CBcol90)
  )
  
  pbps_proj[[sys[["type"]]]] <- lproj
  
}

# Payback Periods ---------------------------------------------------------
PPcol70 <- "PP70"
PPcol90 <- "PP90"

PP_hist <- lapply(names(pbps_hist), function(sys){
  
  df <- STICSdt::join_dfs(list(pbps_hist[[sys]][["70"]],
                               pbps_hist[[sys]][["90"]]),
                          by = c("stncode", "soilcode")) |>
    data.table::as.data.table()
  
  ppdf <- df[, c(1,2)]

  info <- syscosts[[sys]]
  CBcol70 <- sprintf("CByr_70_%s_sum", info[["id"]])
  CBcol90 <- sprintf("CByr_90_%s_sum", info[["id"]])
    
  df[, .(CBnet70 = get(CBcol70), CBnet90 = get(CBcol90))]
  df[, c("system", PPcol70, PPcol90, "CBnet70", "CBnet90", CBcol70, CBcol90):= .(
    sys,
    PP(info[["total.asset"]], get(CBcol70), nyears_hist),
    PP(info[["total.asset"]], get(CBcol90), nyears_hist),
    get(CBcol70),
    get(CBcol90),
    NULL,
    NULL
  )]
  }) |> purrr::list_rbind() |> data.table::as.data.table()

PP_proj <- lapply(names(pbps_proj), function(sys){
  df <- STICSdt::join_dfs(list(pbps_proj[[sys]][["70"]],
                               pbps_proj[[sys]][["90"]]),
                          by = c("model", "ssp", "soilcode")) |>
    data.table::as.data.table()
  
  info <- syscosts[[sys]]
  CBcol70 <- sprintf("CByr_70_%s_sum", info[["id"]])
  CBcol90 <- sprintf("CByr_90_%s_sum", info[["id"]])
  
  df[, .(CBnet70 = get(CBcol70), CBnet90 = get(CBcol90))]
  df[, c("system", PPcol70, PPcol90, "CBnet70", "CBnet90", CBcol70, CBcol90):= .(
    sys,
    PP(info[["total.asset"]], get(CBcol70), nyears_proj),
    PP(info[["total.asset"]], get(CBcol90), nyears_proj),
    get(CBcol70),
    get(CBcol90),
    NULL,
    NULL
  )]
}) |> purrr::list_rbind() |> data.table::as.data.table()

PP_proj_means <- STICSdt::summarise_vars(PP_proj,
                                         var = c("PP70", "PP90"),
                                         type = "mean",
                                         by = c("ssp", "soilcode", "system")) |>
  data.table::as.data.table()

PP_hist[, c("thrsh70", "thrsh90") := .(
          PP70 < 20,
          PP90 < 20)]

PP_proj_means[, c("thrsh70", "thrsh90") := .(
  PP70_mean < 20,
  PP90_mean < 20)]
