packages <- c("SticsRPacks",
              "dplyr",
              "ggplot2",
              "data.table",
              "purrr",
              "STICSdt",
              "SPEI",
              "tidyextreme",
              "lubridate")

invisible(lapply(packages, function(p) {
  if (!(p %in% installed.packages())) {
    if (p == "SticsRPacks") {
      devtools::install_github("SticsRPacks/SticsRPacks@*release")
      devtools::install_github("SticsRPacks/CroPlotR@*release")
    } else if (p == "STICSdt") {
      devtools::install_github("marpols/STICSDataTools", force = TRUE)
    } else {
      install.packages(p, character.only = T)
    }
  }
  library(p, character.only = TRUE)
}))

remove(packages)
