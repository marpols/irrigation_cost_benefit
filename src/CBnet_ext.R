CBnet_ext <- function(df,
                  nyrs,
                  by = c("stncode", "soilcode")){
                    
  syscosts <- irrcosts()
  envcosts <- envcosts()
  
  pbps_ext <- list()
  
  win <- as.data.frame(t(embed(unique(df$ian), nyrs)))
  
  for(sys in syscosts){
    col <- sprintf("%s_SIC", sys[["id"]])
    CBcol70 <- sprintf("CByr_70_%s", sys[["id"]])
    CBcol90 <- sprintf("CByr_90_%s", sys[["id"]])
    
    lvls <- c("min", "mid", "max")
    
    lapply(lvls, function(l){
      CB70 <- sprintf("%s_%s", CBcol70, l)
      CB90 <- sprintf("%s_%s", CBcol90, l)
      
      df[
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
      
      
      
      newlist <- lapply(names(win), function(w){
        sub <- df[ian %in% win[[w]]]
        list("70" = STICSdt::get_sum(as.data.frame(sub),
                                     by = by,
                                     CB70),
             "90" = STICSdt::get_sum(as.data.frame(sub),
                                     by = by,
                                     CB90)
        )
      }) |> purrr::list_flatten()
      
      l70 <- newlist[which(names(newlist) == "70")] |> 
        purrr::list_rbind() |>
        STICSdt::get_mean(var = paste0(CB70, "_sum"),
                          by = by) |>
        dplyr::rename("{CB70}_sum" := paste0(CB70, "_sum_mean"))
      
      l90 <- newlist[which(names(newlist) == "90")] |> 
        purrr::list_rbind() |>
        STICSdt::get_mean(var = paste0(CB90, "_sum"),
                          by = by) |>
        dplyr::rename("{CB90}_sum" := paste0(CB90, "_sum_mean"))
      
      newlist <- list("70" = data.table::as.data.table(l70),
                    "90" = data.table::as.data.table(l90))
      
      pbps_ext[[sys[["type"]]]][[l]] <<- newlist
      
    })
    
  }
  
  invisible(pbps_ext)
}
