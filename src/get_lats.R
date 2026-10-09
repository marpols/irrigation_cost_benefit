get_lats <- function(df,
                    js_path = "C:\\Users\\marpo\\Documents\\Research\\STICS\\JavaSTICS-1.5.3-STICS-10.3.0",
                    ws = "irrigation_assessment"){
  stns <- unique(df$file_name)
  
  lats <- lapply(stns, function(s){
    file <- sprintf("%s\\%s_sta.xml", file.path(js_path, ws), s)
    SticsRFiles::get_param_xml(file, c("latitude"))[[1]]
  }) |> unlist() |> data.table::as.data.table() |> setNames("latitude")
  
  lats[, file_name := stns]
}
