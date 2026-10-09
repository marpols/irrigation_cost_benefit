calc_clim_index <- function(df,
                          index = c("R1mm", "R10mm", "R20mm", "CWD", "CDD", "TX30", "TX35"),
                          by,
                          start_mo = 1,
                          end_mo = 12,
                          ...) {
  index <- match.arg(index)
  func <- switch(
    index,
    "R1mm" = tidyextreme::calculate_R1mm,
    "R10mm" = tidyextreme::calculate_R10mm,
    "R20mm" = tidyextreme::calculate_R20mm,
    "CWD" = tidyextreme::calculate_CWD,
    "CDD" = tidyextreme::calculate_CDD,
    "TX30" = tidyextreme::calculate_TX30,
    "TX35" = tidyextreme::calculate_TX35,
    "TXx" = tidyextreme::calculate_TX25
  )

  allowed_args <- switch(
    index,
    "R1mm" = c("prcp_col", "threshold"),
    "R10mm" = c("prcp_col", "threshold"),
    "R20mm" = c("prcp_col", "threshold"),
    "CWD" = c("prcp_col", "wet_threshold"),
    "CDD" = c("prcp_col", "dry_threshold"),
    "TX30" = c("tmax_col"),
    "TX35" = c("tmax_col"),
    "TXx" = c("tmax_col, threshold")
  )

  args <- list(...)

  invalid_names <- setdiff(names(args), allowed_args)
  if (length(invalid_names) > 0) {
    warning("Invalid optional argument(s): ", paste(invalid_names, collapse = ", "))
    return()
  }

  if (!"Date" %in% names(df)) {
    df$Date <- lubridate::make_date(df$ian, df$mo, df$jo)
  }

  tryCatch({
    df |>
      dplyr::filter(mo >= start_mo, mo <= end_mo) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::group_modify(
        \(data, keys) {
          do.call(
            func,
            c(
              list(df = data, time_col = "Date"),
              args
            )
          )
        }
      ) |>
      dplyr::ungroup()
  }, error = function(e) {

  })

}
