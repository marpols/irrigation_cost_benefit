get.mods.files <- function(dir = "",
                           usms. = usms,
                           version = "",
                           group_by = NULL) {
  
  dirs <- list.dirs(file.path(javastics_path, workspace, dir),
                    full.names = TRUE,
                    recursive = F) |>
    grep(sprintf("%s$|%s_", version, version), x = _, value = TRUE)
  
  ver_usms <- grep(sprintf("%s$", version), usms., value = T)
  
  group <- if(is.null(group_by)) unique(get.group.name(ver_usms)) else group_by
  
  all_sims <- list()
  
  for (d in dirs) {
    
    name <- str_extract(d, sprintf("%s\\w*$", version))
    
    files <- list.files(
      file.path(d),
      full.names = TRUE,
      recursive = TRUE
    ) |>
      grep("mod_s", x = _, value = TRUE)
    
    
    files <- lapply(group, function(g) {
      grep(g, files, value = T)
    })
    
    sims <- lapply(files, function(f){
      group_sims <- lapply(f, read.csv, sep = ";")
      names(group_sims) <- lapply(f, get.file.name)
      lapply(group_sims, function(c) {
        class(c) <- c("STICS simulation", "data.frame")
        c
      })
    })

    names(sims) <- group
    
    all_sims[[str_extract(d, "\\w*$")]] <- sims
  }
  
  return(all_sims)
  
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

  writexl::write_xlsx(list, file.path(outdir, fname))
}

save.to.csv <- function(list, outdir, grouping){
  new_out <- file.path(outdir,"csv files")
  dir.create(new_out)
  
  file.names <- paste0(names(list),".csv")
  
  mapply(write.table, list, file.names)
}




