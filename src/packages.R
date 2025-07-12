packages <- c("SticsRPacks", "stringr", "dplyr", "ggplot2", "rlang", "scales",
              "readxl", "writexl", "purrr", "precintcon", "tidyr")

invisible(lapply(packages, function(p) {
  if (!(p %in% installed.packages())) {
    if (p == "SticsRPacks") {
      devtools::install_github("SticsRPacks/SticsRPacks@*release")
      devtools::install_github("SticsRPacks/CroPlotR@*release")
    } else {
      install.packages(p, character.only = T)
    }
  }
  library(p, character.only = TRUE)
}))
