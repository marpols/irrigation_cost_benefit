get.weather.station <- function(usm){
  return(sub("\\..*$", "", basename(usm$paths[3])))
}

get.longterm.climate <- function(source){
  station <- if(class(source) == "list") get.weather.station(source) else source
  
  station <- gsub("\\(", "\\\\(", station)
  
  climate_files <- list.files(file.path(javastics_path,workspace),
                              full.names = T) |> grep(sprintf("%s.\\d{4}", 
                                                              station),
                                                      x = _,
                                                      value = TRUE)
  
  all_climate <- do.call(rbind, lapply(climate_files, 
                                       read.csv, 
                                       header = FALSE,
                                       sep="\t"))
  
  names(all_climate) <- c("station", 
                       "ian", 
                       "mo", 
                       "jo", 
                       "jul", 
                       "min.temp", 
                       "max.temp", 
                       "solar.rad", 
                       "PET", 
                       "precip.", 
                       "windspeed", 
                       "vapour.pressure", 
                       "CO2")
  
  all_climate <- calculate.gdd(all_climate)
  
  return(all_climate)
}

calculate.gdd <- function(climfile, base = 5){
  climfile$GDD <- pmax(0,
                        (climfile$max.temp + climfile$min.temp) / 2 - base)
  climfile
}

get.monthly.avgs <- function(climfile){
  
  mavg <- climfile |>  group_by(ian, mo) |>
    summarize(
      min.temp = mean(min.temp, na.rm = TRUE, ),
      max.temp = mean(max.temp, na.rm = TRUE, ),
      precip.cum = sum(precip., na.rm = TRUE),
      windspeed = mean(windspeed, na.rm = TRUE),
      vapour.pressure = mean(vapour.pressure, na.rm = TRUE),
      radiation = mean(solar.rad, na.rm = TRUE),
      GDD = sum(GDD, na.rm = TRUE)
    )
  
  return(mavg)
}

get.summary <- function(mavgs, months){
  
  gs <- mavgs[mavgs$mo %in% months,]
  
  gs_mean <- gs |> group_by(ian) |>
    summarise(
      min.temp = mean(min.temp, na.rm = TRUE, ),
      max.temp = mean(max.temp, na.rm = TRUE, ),
      precip.cum = sum(precip.cum, na.rm = TRUE),
      windspeed = mean(windspeed, na.rm = TRUE),
      vapour.pressure = mean(vapour.pressure, na.rm = TRUE),
      radiation = mean(radiation, na.rm = TRUE),
      GDD = sum(GDD, na.rm = TRUE)
    )
  
  return(gs_mean)
} 

longterm.avg <- function(clim_summary){
  
  df <- clim_summary |> 
    summarise(min.temp = mean(min.temp, na.rm = TRUE, ),
              max.temp = mean(max.temp, na.rm = TRUE, ),
              precip.cum = mean(precip.cum, na.rm = TRUE),
              vapour.pressure = mean(vapour.pressure, na.rm = TRUE),
              radiation = mean(radiation, na.rm = TRUE),,
              GDD = mean(GDD, na.rm = TRUE))
  
  return(df)
  
}

create.climate.history <- function(clim_src, new_out){
  
  climfile <- get.longterm.climate(clim_src)
  
  station <- if(class(clim_src) == "list") unique(climfile$station) else clim_src
  
  write.table(climfile, file.path(new_out,
                                  sprintf("%s_climatefile.csv", station)))
  saveRDS(climfile, file.path("data",
                              sprintf("%s_climatefile.RDS", station)))
  
  monthly <- get.monthly.avgs(climfile)
  monthly$station <- station
  
  gs_summary <- get.summary(monthly, 6:9)
  julaug_summary <- get.summary(monthly, c(7,8))
  
  gs_hist_avg <- longterm.avg(gs_summary)
  julaug_hist_avg <- longterm.avg(julaug_summary)
  
  gs_summary$station <- station
  gs_summary$period <- "GS"
  julaug_summary$station <- station
  julaug_summary$period <- "julaug"
  gs_hist_avg$station <- station
  gs_hist_avg$period <- "GS"
  julaug_hist_avg$station <- station
  julaug_hist_avg$period <- "julaug"
  
  summaries <- rbind(gs_summary, julaug_summary)
  hist_averages <- rbind(gs_hist_avg, julaug_hist_avg)
  
  return(setNames(
    list(list(
      monthly = monthly,
      yearly = summaries,
      historical_avgs = hist_averages
    )),
    station
  ))
}

load.climate <- function(){
  
  new_out <- file.path(outdir, "climate")
  if(!dir.exists(new_out)) dir.create(new_out)
  
  climate <- lapply(unique(lapply(usm_files, get.weather.station)),
                    create.climate.history) |> flatten()
  climate_dir <- file.path(outdir, "climate")
  mapply(function(cfile, cname){
    writexl::write_xlsx(cfile,
                        file.path(climate_dir,sprintf("%s_summary.xlsx",cname)))
    saveRDS(cfile, file.path("data",sprintf("%s_summary.RDS",cname)))
  }, climate, names(climate))
  
  lapply(c("monthly","yearly","historical_avgs"), function(id){
    list <- lapply(climate, `[[`, id)
    aggregated <- do.call(rbind, list)
    saveRDS(aggregated, file.path("data", sprintf("%s.RDS",id)))
  })
  
}
