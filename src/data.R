organise.data <- function(sim_list, index, parameters,
                          group_by = NULL,
                          fname="",
                          summary_function=""){
  #organises data from a list of sims.
  #list should have two elements: [[1]] stressed [[2]] no stress
  #each element is a list of sims organised by a chosen grouping
  
  writedir <- file.path(outdir, names(sim_list)[[index]])
  if (!dir.exists(writedir)) dir.create(writedir)
  
  simRDS <- sprintf("data/%s_sims.RDS", names(sim_list)[[index]])
  if (!file.exists(simRDS)) saveRDS(sim_list, simRDS)
  
  monthly <- readRDS("data/monthly.RDS")
  yearly <- readRDS("data/yearly.RDS")
  historical <- readRDS("data/historical_avgs.RDS")
  
  if(summary_function == "yield.data"){
  yield_summary <- summarise.yield.data(sim_list, group_by, writedir) |>
    add.climate(clim = yearly) |>
    add.CWR(sims = sim_list) |>
    calculate.costs() |>
    calculate.gross.benefit()
  
  cols_to_front <- c("ian", "soil", "stn_code", "period")
  summary <- lapply(yield_summary, function(y){
    y[, c(cols_to_front,
              setdiff(names(y),
                      cols_to_front))]
  })} else {
    summary <- summarise.data(sim_list, index, parameters)
  }
  
  saveRDS(summary, sprintf("data/%s_%s_summary.RDS", 
                                 str_extract(writedir, "\\w*$"),
                                 fname))
  save.to.xl(summary, writedir, fname)
  
  aggregated <- do.call(rbind, summary)
  aggregated$scenario <- str_extract(rownames(aggregated),"\\w*")
  write.table(aggregated,
              file = file.path(writedir,
                               sprintf("%s_%s_all.csv", 
                                       str_extract(writedir, "\\w*$"),
                                       fname)), 
              row.names = F, sep = ",")
  
  saveRDS(aggregated, sprintf("data/%s_%s_all.RDS", 
                              str_extract(writedir, "\\w*$"),
                              fname))
}

summarise.yield.data <- function(sim_list, group_by, out_dir){
  
  #stressed
  stressed_sims <- sim_list[[1]]
  stressed_yields <- lapply(stressed_sims, get.yields)
  average_stressed_yields <- lapply(stressed_sims, avg.yield)
  stressed_mrktyld_low <- lapply(stressed_sims, function(sim_s) {
    lapply(sim_s, calc.market.yield, percent = "low")
  })
  stressed_mrktyld_high <- lapply(stressed_sims, function(sim_s) {
    lapply(sim_s, calc.market.yield, percent = "high")
  })
  
  #unstressed
  unstressed_sims <- sim_list[[2]]
  unstressed_yields <- lapply(unstressed_sims, get.yields)
  average_unstressed_yields <- lapply(unstressed_sims, avg.yield)
  unstressed_mrktyld_low <- lapply(unstressed_sims, function(sim_s) {
    lapply(sim_s, calc.market.yield, percent = "low")
  })
  unstressed_mrktyld_high <- lapply(unstressed_sims, function(sim_s) {
    lapply(sim_s, calc.market.yield, percent = "high")
  })
  
  no_irr <- readRDS("data/denit__all.RDS")
  irr_30mm <- readRDS("data/denit_autoIrr__all.RDS")
  irr_20mm <- readRDS("data/denit_autoIrr_20mm__all.RDS")
  no_irr$irrigation <- 0
  irr_30mm$irrigation <- irrigation <- 30
  irr_20mm$irrigation <- 20
  
  no_irr$yieldgain_30mm <- pmax(irr_30mm$yield - no_irr$yield, 0)
  no_irr$yieldgain_20mm <- pmax(irr_20mm$yield - no_irr$yield, 0)
  sum(no_irr$yieldgain_30mm > no_irr$yieldgain_20mm)
  sum(no_irr$yieldgain_20mm > no_irr$yieldgain_30mm)
  
  no_irr$changeQles_30mm <- irr_30mm$Qles - no_irr$Qles
  no_irr$changeQles_20mm <- irr_20mm$Qles - no_irr$Qles
  no_irr$changeN2O_30mm <- irr_30mm$Qem_N2O - no_irr$Qem_N2O
  no_irr$changeN2O_20mm <- irr_20mm$Qem_N2O - no_irr$Qem_N2O
  
  
  #combine all
  stressed_yields_df <- to.df(stressed_yields)
  unstressed_yields_df <- to.df(unstressed_yields)
  stressed_mrktyld_low_df <- to.df(stressed_mrktyld_low)
  unstressed_mrktyld_low_df <- to.df(unstressed_mrktyld_low)
  stressed_mrktyld_high_df <- to.df(stressed_mrktyld_high)
  unstressed_mrktyld_high_df <- to.df(unstressed_mrktyld_high)

  combined <- list()
  
  i <- length(stressed_yields_df)
  while (i > 0){
    df <- data.frame(stressed_yields_df[[i]],
                     unstressed_yields_df[[i]],
                     stressed_mrktyld_low_df[[i]],
                     unstressed_mrktyld_low_df[[i]],
                     stressed_mrktyld_high_df[[i]],
                     unstressed_mrktyld_high_df[[i]]
    )
    colnames(df) <- c("Yield, stress",
                      "Yield, no stress",
                      "Low market yield, stress", 
                      "Low market yield, no stress",
                      "High market yield, stress", 
                      "High market yield, no stress")
    
    df$Gains.total <- pmax(df$`Yield, no stress` - df$`Yield, stress`,0)
    df$Gains.market.low <- pmax(df$`Low market yield, no stress`- 
                                 df$`Low market yield, stress`,0)
    df$Gains.market.high <- pmax(df$`High market yield, no stress`- 
                                  df$`High market yield, stress`,0)
    
    
    combined[[names(stressed_yields_df)[[i]]]] <- df
    
    i <- i - 1
  }
  
  return(combined)
}

to.df <- function(sim_list){
  #convert a list from list of sims to data.frame
  
  lapply(sim_list, function(sl){
    df <- t(as.data.frame(sl))
    colnames(df) <- unique(row.names(df) |>
      str_remove_all("^X\\d{4}_") |>      
      str_remove_all("_hills\\d?$"))
    row.names(df) <- get.years(row.names(df))
    df
  })
}

add.climate <- function(yields, clim){
  
  clim$stn_code <- lapply(clim$station, get.stn.code) |> unlist()
  names <- names(yields)
  
  mapply(function(y,n){
    ids <- str_split(n,"_") |> unlist()
    y$ian <- as.integer(row.names(y))
    y$soil <- ids[2]
    y$stn_code <- ids[1]
    y <- left_join(y, clim, by = c("ian", "stn_code"))
    y
  }, yields, names, SIMPLIFY = FALSE)
  
}

add.CWR <- function(yields, sims){
  sims <- sims[[1]]
  CWR <- lapply(sims,function(s){
    lapply(s, calc.water.requirements)
  })
  x <- lapply(CWR, function(id_code){
    names <- names(id_code)
    
    result <- mapply(function(wr, n){
      ids <- str_split(n,"_") |> unlist()
      wr$ian <- as.integer(ids[1])
      wr$stn_code <- ids[2]
      wr$soil <- ids[3]
      cols <- c("cetm", "precip_e", "CWR")
      wr[cols] <- lapply(wr[cols], as.double)
      wr
    },id_code, names, SIMPLIFY = FALSE)
    
    ids <- str_split(names,"_")[[1]] |> unlist()
    id <- paste0(ids[2],"_", ids[3])
    df <- left_join(yields[[id]], dplyr::bind_rows(result), by = c("ian",
                                                                  "stn_code",
                                                                  "soil",
                                                                  "period"))
    df
    
  })
}

calculate.costs <- function(data){
  
  irrigation_systems <- irrigation.costs()
  
  costs <- lapply(data,
                  function(x) lapply(irrigation_systems,
                                           calc.sic, data = x)) |>
    lapply(function(c){as.data.frame(do.call(cbind, c))})
  
  mapply(function(d,c){
    cbind(d,c)
  }, data, costs, SIMPLIFY = F)
}

calculate.gross.benefit <- function(data){
  lapply(data, function(d){
    d$irrigated <- d$precip_e < d$cetm
    d$"Gross Benefit, low" <- calc.earnings(d$Gains.market.low)
    d$"Gross Benefit, high" <- calc.earnings(d$Gains.market.high) 
    d[d$irrigated == F, "Gross Benefit, low"] <- 0
    d[d$irrigated == F, "Gross Benefit, high"] <- 0
  })
}

cost.benefit.data <- function(data, fname = ""){
  #caluculate cummulative gross benefit per rotation
  gs_gross_benefit <- lapply(data, function(df){
    df <- df |> filter(period == "GS")
    get.cost.benefit(df)
  })
  
  julaug_gross_benefit <- lapply(data, function(df){
    df <- df |> filter(period == "julaug")
    get.cost.benefit(df)
  })
  
  writedir <- file.path(outdir, fname)
  if (!dir.exists(writedir)) dir.create(writedir)
  
  save.to.xl(gs_gross_benefit, writedir, "gs")
  save.to.xl(julaug_gross_benefit, writedir, "julaug")
  
  saveRDS(gs_gross_benefit, "data/gs_gross_benefit.RDS")
  saveRDS(julaug_gross_benefit, "data/julaug_gross_benefit.RDS")
  
  gs_aggregated <- do.call(rbind, gs_gross_benefit)
  julaug_aggregated <- do.call(rbind, julaug_gross_benefit)
  
  all_aggregated <- rbind(gs_aggregated, julaug_aggregated)
  
  
  write.table(all_aggregated,
              file = file.path(writedir,
                               sprintf("%s_%s_all.csv", 
                                       str_extract(writedir, "\\w*$"),
                                       fname)), 
              row.names = F, sep = ",")
  
  saveRDS(all_aggregated, sprintf("data/%s_%s_all.RDS", 
                              str_extract(writedir, "\\w*$"),
                              fname))
  
  
}

summarise.data <- function(sim_list, index, parameters){
  sims <- sim_list[[index]]
  yields <- lapply(sims, get.yields) |> to.df()
  average_yields <- lapply(sims, avg.yield) |> to.df()
  mrktyld_low <- lapply(sims, function(sim_s) {
    lapply(sim_s, calc.market.yield, percent = "low") 
  }) 
  mrktyld_high <- lapply(sims, function(sim_s) {
    lapply(sim_s, calc.market.yield, percent = "high")
  }) 
  data <- lapply(parameters, function(param){
    print(param)
    lapply(sims, get.data, param) |> to.df()
  }) 
  data <- setNames(data, parameters) 
  
  combined <- list()
  
  for (p in names(yields)){
    combined[[p]] <- data.frame(yields[[p]],
                                lapply(parameters,function(param)
                                  data[[param]][[p]]
                                ))
   names(combined[[p]]) <- c("yield",parameters)
  }
  
  return(combined)
  
}

get.data <- function(sims, param){
  return( 
    lapply(sims, function(x){
    max(x[[param]])
  })
  )
}

#Reading mod b files
get.modb.files <- function(dir){
  files <- list.files(dir,"mod_b", full.names=TRUE, recursive=TRUE)
  f <- lapply(files, readLines) 
  names(f) <- str_extract(files, "\\d+_[A-Z]+_[A-Z]+")
  return(f)
}

get.irr.info <- function(txt_list){
  scen <- names(txt_list) |> str_replace("\\d+_", "") |> unique()
  info <- lapply(scen,
                 function(s){
                   group <- txt_list[grep(s, names(txt_list))]
                   years <- names(group) |> 
                     str_replace(paste("_",s,sep=""),"") |> as.numeric()
                   lapply(group, function(g){
                     list("app_irr" = get.auto.irr(g),
                          "precip+irr" = get.precip.irr(g),
                          "irr_dates" = get.irr.dates(g))
                   })
                 })
  names(info) <- scen
  #saveRDS(info, "data/irr_max30.RDS")
}

get.auto.irr <- function(txt){
  str <- regmatches(txt,
                    regexpr("irrigation\\s*\\d+",
                            txt))[1]
  return(as.numeric(str_split(str,
                              "irrigation\\s*")[[1]][2]))
}

get.precip.irr <- function(txt){
  #gets cumulative precip + irrigation over crop lifespan (not simulation period) 
  str <- regmatches(txt, 
                    regexpr("Cumulative Rainfall\\+irrigation\\s=\\s*\\d+",
                            txt))[1]
  return(as.numeric(str_split(str,
                              "Cumulative Rainfall\\+irrigation\\s=\\s*")[[1]][2]))
}

get.irr.dates <- function(txt){
  i <- grep("Irrigation",txt)
  if(is_empty(i)){
    c <- 0
  } else {
    dates <- txt[(i[1]+1):(i[2]-1)]
    c <- c()
    for (d in dates){
      s <- str_split(d, 
                     "\\s+")
      date <- as.Date(s[[1]][2],format = "%d-%b-%Y")
      amount <- as.numeric(s[[1]][3])
      c <- c(c, as.Date(date), amount)
    }
  }
  return(c)
}

