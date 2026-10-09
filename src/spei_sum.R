spei_sum <- function(df,
                     mo = 8,
                     var = "stncode",
                     by = "stncode"){
  
  classes <- c("Extremely Wet",
               "Very Wet",
               "Moderate Wet",
               "Normal",
               "Moderate Dry",
               "Very Dry",
               "Extremely Dry")
  
  speimo <- df[MONTH == mo][,ian := YEAR]
  names <- unique(df[[var]])

  
  nyrs <- speimo[
    , .(n_years = .N), by = c(by, "class")
  ][
    , class := factor(class, levels = classes)
  ] |>
    data.table::dcast(
      as.formula(paste("class ~", paste(by, collapse = " + "))),
      value.var = "n_years",
      fill = 0L,
      drop = FALSE
    )
  
  nyrs[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = names(nyrs)[-1]][
    , median := apply(.SD, 1, median, na.rm = TRUE), .SDcols = names(nyrs)[-1]
  ]
  
}













