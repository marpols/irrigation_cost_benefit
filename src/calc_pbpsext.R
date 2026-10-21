calc_pbpsext <- function(dtl,
                         by = c("stncode", "soilcode"),
                         means = FALSE,
                         nyrs = 20) {
  
  syscosts <- irrcosts()
  lvls <- c("min", "mid", "max")
  
  pbps_ext <- lapply(names(dtl), function(sys) {
    
    info <- syscosts[[sys]]
    
    lapply(lvls, function(l) {
      df <- STICSdt::join_dfs(list(dtl[[sys]][[l]][["70"]], dtl[[sys]][[l]][["90"]]), by = by) |>
        data.table::as.data.table()
      
      PPcol70 <- sprintf("PP70_ext_%s", l)
      PPcol90 <- sprintf("PP90_ext_%s", l)
      CB70 <- sprintf("CByr_70_%s_%s_sum", info[["id"]], l)
      CB90 <- sprintf("CByr_90_%s_%s_sum", info[["id"]], l)
      CBnet70 <- sprintf("CBnet70_%s", l)
      CBnet90 <- sprintf("CBnet90_%s", l)
      
      df[, c("system", PPcol70, PPcol90, CBnet70, CBnet90, CB70, CB90) := .(
        sys,
        PP(info[["total.asset"]], get(CB70), nyrs),
        PP(info[["total.asset"]], get(CB90), nyrs),
        get(CB70),
        get(CB90),
        NULL,
        NULL
      )]
    }) |> (\(tables) Reduce(\(x, y) merge(
      x, y, by = c(by, "system"), all = TRUE
    ), tables))()
    
  }) |> purrr::list_rbind() |> data.table::as.data.table()
  
  
  if (means) {
    pbps_ext_means <- STICSdt::summarise_vars(
      pbps_ext,
      var = c(grep("PP", names(pbps_ext), value = TRUE),
              grep("CB", names(pbps_ext), value = TRUE)),
      type = "mean",
      by = c("ssp", "soilcode", "system")
    ) |>
      data.table::as.data.table()
  } else {
    pbps_ext_means <- FALSE
  }
  
  # pbps_ext[, c("thrsh70", "thrsh90") := .(
  #   PPcol70 < 20,
  #   PPcol90 < 20)]
  
  invisible(list(pbps = pbps_ext, pbps_mean = pbps_ext_means))
  
}