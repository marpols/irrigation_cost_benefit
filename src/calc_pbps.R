calc_pbps <- function(l,
                      by = c("stncode", "soilcode"),
                      means = FALSE){
  PPcol70 <- "PP70"
  PPcol90 <- "PP90"
  syscosts <- irrcosts()

  pbps <- lapply(names(l), function(sys){

    dt <- STICSdt::join_dfs(list(l[[sys]][["70"]],
                                 l[[sys]][["90"]]),
                            by = by) |>
      data.table::as.data.table()
    
    ppdf <- dt[, c(1,2)]
    
    info <- syscosts[[sys]]
    CBcol70 <- sprintf("CByr_70_%s_sum", info[["id"]])
    CBcol90 <- sprintf("CByr_90_%s_sum", info[["id"]])
    
    dt[, .(CBnet70 = get(CBcol70), CBnet90 = get(CBcol90))]
    dt[, c("system", PPcol70, PPcol90, "CBnet70", "CBnet90", CBcol70, CBcol90):= .(
      sys,
      PP(info[["total.asset"]], get(CBcol70), nyrs),
      PP(info[["total.asset"]], get(CBcol90), nyrs),
      get(CBcol70),
      get(CBcol90),
      NULL,
      NULL
    )]
  }) |> purrr::list_rbind() |> data.table::as.data.table()
  
  
  if (means) {
    pbps_means <- STICSdt::summarise_vars(pbps,
                                        var = c("PP70", "PP90"),
                                        type = "mean",
                                        by = c("ssp", "soilcode", "system")) |>
      data.table::as.data.table()
    
    pbps_means[, c("thrsh70", "thrsh90") := .(PP70_mean < 20, PP90_mean < 20)]
  } else{
    pbps_means = NULL
  }
  
  pbps[, c("thrsh70", "thrsh90") := .(
    PP70 < 20,
    PP90 < 20)]
  
  invisible(list(pbps = pbps, pbps_mean = pbps_means))
}
