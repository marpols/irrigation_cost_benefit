get.mods.files <- function(){
   
  files <- list.files(file.path(javastics_path, workspace, "Results"),
                      full.names = TRUE,
                      recursive = TRUE) |> 
     grep("mod_s", x = _, value = TRUE)
   
   sims <- lapply(files, read.csv, sep = ";")
   
   usms <- str_extract(files, "(?<=/)mod_s(.*?)\\.sti$") |>
     str_remove("^mod_s") |>
     str_remove("\\.sti$")
   
   usms[str_detect(files, "noWS")] <- paste0(usms[str_detect(files, "noWS")],
                                             "_noWS")
   
   names(sims) <- usms
   
   return(sims)
   
}

get.groups <- function(){
  usms <- names(sims)
  
  groups <- usms |>
    str_remove_all("^PEI_") |>        # remove leading "PEI_"
    str_remove_all("^[0-9]{4}_") |>   # remove year at beginning
    str_remove_all("_hills") |>      # remove trailing "_hills"
    str_remove_all("_noWS$") 
  
  return(unique(groups))
}

