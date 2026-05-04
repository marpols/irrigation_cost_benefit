get_mods <- function(dir = "RESULTS",
                           usm_list = usms,
                           exp_id = "",
                           run_id = "",
                           ver_num = NULL,
                           stn_code = "",
                           soil_code = "",
                           ssp = "",
                           group = "") {
  
  exp_dir <- ifelse(is.null(ver_num),
                     sprintf("%s_%s", exp_id, run_id),
                     sprintf("%s_%s_%d", exp_id, run_id, ver_num)) 
  
  exp_files <- list.files(file.path(javastics_path, 
                                 workspace, 
                                 dir, 
                                 exp_dir),
                    full.names = TRUE,
                    recursive = TRUE) |>
    grep("mod_s", x=_, value = TRUE)
  
  if(stn_code != ""){
    exp_files <- grep(stn_code, exp_files, value = TRUE)
  }
  if(soil_code != ""){
    exp_files <- grep(soil_code, exp_files, value = TRUE)
  }
  if(ssp != ""){
    exp_files <- grep(ssp, exp_files, value = TRUE)
  }
  if (group != ""){
    exp_files <- grep(group, exp_files, value = T)
  }
  
  sims <- lapply(exp_files, function(d){
    name <- str_extract(d, "\\w*-*\\w*-*\\w*.sti") |>
      str_remove("mod_s") |> str_remove(".sti")
    ids <- str_split(name, "_") |> unlist()
    mod_s <- read.csv(d, sep=";")
    mod_s["soil_code"] <- ifelse(length(ids) == 6, ids[5], ids[4])
    mod_s["stn_code"] <- ifelse(length(ids) == 6, ids[4], ids[3])
    mod_s["model"] <- ifelse(length(ids) == 6, ids[1], NA)
    mod_s["ssp"] <- ifelse(length(ids) == 6, ids[2], NA)
    mod_s["file_name"] <- name
    mod_s <- mod_s |> relocate("stn_code", "soil_code", "model", "ssp")
    #class(mod_s) <- c("STICS simulation", "data.frame")
    return (mod_s)
  })
  
  return(purrr::list_rbind(sims))
}

get.groups <- function(sims){
  usms <- names(sims)
  
  groups <- get.group.name(usms) 
  
  return(unique(groups))
}

get.file.name <- function(files){
  str_extract(files[[1]], "\\d{4}\\w*")
}

get.group.name <- function(usm_name){
  
  usm_name <- usm_name |>
    str_remove_all("^PEI\\d?_") |>        # remove leading "PEI_"
    str_remove_all("^[0-9]{4}_") |>   # remove year at beginning
    str_remove_all("_hills\\d?") |>      # remove trailing "_hills"
    str_remove_all("_noWS$")
  
  return(usm_name)
}

get.years <- function(usm_names){
  years <- regmatches(usm_names, regexpr("\\d{4}", usm_names))
  return(years)
}

get.stn.code <- function(station){
  unlist(regmatches(station,
                    gregexpr("(?<=^|_)[A-Z]",
                             station, perl = TRUE))) |>
    paste(collapse = "")
}

get.stn <- function(stn_code){
  if(stn_code == "S"){
    return("SUMMERSIDE")
  } else if(stn_code == "HCC"){
    return("HARRINGTON_CDA_CS")
  } else if(stn_code == "EP"){
    return("EAST_POINT_(AUT)")
  } else if(stn_code == "NG"){
    return("NEW_GLASGOW")
  }
}

save.to.xl <- function(list, outdir, fname){
  #list = list of data.frames
  fname <- paste0(str_extract(outdir, "\\w*$"), "_", fname, ".xlsx")

  openxlsx::write.xlsx(list, file.path(outdir, fname))
}

save.to.csv <- function(list, outdir, grouping){
  new_out <- file.path(outdir,"csv files")
  dir.create(new_out)
  
  file.names <- paste0(names(list),".csv")
  
  mapply(write.table, list, file.names)
}




