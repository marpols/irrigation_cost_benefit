CBnet <- function(df,
                   nyrs,
                   by = c("stncode", "soilcode")){
  
  syscosts <- irrcosts()
  
  pbps <- list()
  
  win <- as.data.frame(t(embed(unique(df$ian), nyrs)))
  
  for(sys in syscosts){
    col <- sprintf("%s_SIC", sys[["id"]])
    CBcol70 <- sprintf("CByr_70_%s", sys[["id"]])
    CBcol90 <- sprintf("CByr_90_%s", sys[["id"]])
    
    df[
      , c(CBcol70, CBcol90) := .(
        CB(gross_70, get(col)),
        CB(gross_90, get(col))
      )
    ]
    
    newlist <- lapply(names(win), function(w){
      sub <- df[ian %in% win[[w]]]
      list("70" = STICSdt::get_sum(as.data.frame(sub),
                                   by = by,
                                   CBcol70),
           "90" = STICSdt::get_sum(as.data.frame(sub),
                                   by = by,
                                   CBcol90)
      )
    }) |> purrr::list_flatten()
    
    l70 <- newlist[which(names(newlist) == "70")] |> 
      purrr::list_rbind() |>
      STICSdt::get_mean(var = paste0(CBcol70, "_sum"),
                        by = by) |>
      dplyr::rename("{CBcol70}_sum" := paste0(CBcol70, "_sum_mean"))
    
    l90 <- newlist[which(names(newlist) == "90")] |> 
      purrr::list_rbind() |>
      STICSdt::get_mean(var = paste0(CBcol90, "_sum"),
                        by = by) |>
      dplyr::rename("{CBcol90}_sum" := paste0(CBcol90, "_sum_mean"))
    
    newlist <- list("70" = data.table::as.data.table(l70),
                  "90" = data.table::as.data.table(l90))
    
    pbps[[sys[["type"]]]] <- newlist 
    
  }
  invisible(pbps)
}

