organise.data <- function(sim_list, group_by = NULL, fname){
  #organises data from a list of sims.
  #list should have two elements: [[1]] stressed [[2]] no stress
  #each element is a list of sims organised by a chosen grouping
  
  writedir <- file.path(outdir, names(sim_list)[[1]])
  if (!dir.exists(writedir)) dir.create(writedir)
  
  simRDS <- sprintf("data/%s_sims.RDS", names(sim_list)[[1]])
  if (!file.exists(simRDS)) saveRDS(sim_list, simRDS)
  
  monthly <- readRDS("data/monthly.RDS")
  yearly <- readRDS("data/yearly.RDS")
  historical <- readRDS("data/historical_avgs.RDS")
  
  
  yield_summary <- summarise.yield.data(sim_list, group_by, writedir) |>
    add.climate(clim = yearly) |>
    add.CWR(sims = sim_list) |>
    calculate.costs() |>
    calculate.gross.benefit()
  
  cols_to_front <- c("ian", "soil", "stn_code", "period")
  yield_summary <- lapply(yield_summary, function(y){
    y[, c(cols_to_front,
              setdiff(names(y),
                      cols_to_front))]
  })
  
  saveRDS(yield_summary, sprintf("data/%s_%s_summary.RDS", 
                                 str_extract(writedir, "\\w*$"),
                                 fname))
  save.to.xl(yield_summary, writedir, fname)
  
  aggregated <- do.call(rbind, yield_summary)
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
    d
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
