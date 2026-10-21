
# Yearly Precip -----------------------------------------------------------


hist_precip <- lapply(list(climhist_summary, climhist_summarygs), \(dt){
  lapply(list("S", "NG", "HCC", "EP"), \(stn){
    STICSdt::plot_climate(dt[stncode == stn,],
                          hist_avg = mean(dt[stncode == stn,
                                             Precipitation_cum_avg]),
                          variable = "Precipitation_cum_avg",
                          title = stn,
                          line_label = "mean")
  }) |> purrr::set_names(c("S", "NG", "HCC", "EP")) 
}) |> purrr::set_names("annual", "gs")

hist_precip

proj_precip <- mapply(\(dt_list,m) {
  
  lapply(dt_list, \(dt){
  dt <- dt |> dplyr::summarise(
    "Precipitation_cum_avg" = mean(Precipitation_cum_avg),
    .by = c("ssp", "ian")
  ) |> data.table::as.data.table()
  
  lapply(list("ssp126", "ssp370", "ssp585"), \(s) {
    STICSdt::plot_climate(
      dt[ssp == s, ],
      hist_avg = m,
      variable = "Precipitation_cum_avg",
      title = s,
      line_label = "historical mean"
    )
  }) |> purrr::set_names(c("ssp126", "ssp370", "ssp585"))
  }) |> purrr::set_names(c("proj", "REF"))
}, list(annual = list(climproj_summary,
                      climREF_sum),
        gs = list(climproj_summarygs,
                  climREF_sumgs)),
list(mean(climhist_summary[,Precipitation_cum_avg]),
     mean(climhist_summarygs[,Precipitation_cum_avg])),
SIMPLIFY = FALSE) |> 
  purrr::set_names("annual", "gs")

proj_precip

# July-Aug Precip ---------------------------------------------------------

julaughist <- climate_hist[mo == 7 | mo == 8,] |>
  dplyr::summarise("Precipitation_cum" = sum(Precipitation),
                                .by = c("stncode", "ian")) |>
  data.table::as.data.table()

julaugproj <- climate_proj[mo == 7 | mo == 8,] |>
  dplyr::summarise("Precipitation_cum" = sum(Precipitation),
                   .by = c("ssp", "model", "ian")) |>
  dplyr::summarise("Precip_avg" = mean(Precipitation_cum),
                   .by = c("ssp", "ian")) |>
  data.table::as.data.table()

lapply(list("S", "NG", "HCC", "EP"), \(stn){
  STICSdt::plot_climate(
  julaughist[stncode == stn, ],
  hist_avg = mean(julaughist[stncode == stn, Precipitation_cum]),
  variable = "Precipitation_cum",
  title = sprintf("%s - July/Aug Precipitation", stn),
  line_label = "historical mean"
) + ggplot2::coord_cartesian(ylim = c(0,325))
}) |> purrr::set_names(c("S", "NG", "HCC", "EP"))


lapply(list("ssp126", "ssp370", "ssp585"), \(s){
  STICSdt::plot_climate(
    julaugproj[ssp == s, ],
    hist_avg = mean(julaughist[,Precipitation_cum]),
    variable = "Precip_avg",
    title = sprintf("%s - July/Aug Precipitation", s),
    line_label = "historical mean"
  ) + ggplot2::coord_cartesian(ylim = c(0,325))
}) |> purrr::set_names(c("ssp126", "ssp370", "ssp585"))
