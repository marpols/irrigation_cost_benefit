
results <- lapply(
  list(hist = data_hist, proj = data_proj),
  function(x) {
    lapply(x[c("fixed", "dynamic")], function(sublist) {
      yearly_SIC(sublist$analysis) |>
      yearly_gross()
      
    })
  }
)


results_hist <- lapply(results$hist, \(df) CBnet(df, 20) |> calc_pbps())
results_proj <- lapply(results$proj, \(df) CBnet(df, 20,
                                                 by = c("model", "ssp", "soilcode"))|>
                         calc_pbps(by = c("model", "ssp", "soilcode"),
                                   means = TRUE))



# With Externalities ------------------------------------------------------

results_histext <- lapply(results$hist, \(df) CBnet_ext(df, 20) |> calc_pbpsext())
results_projext <- lapply(results$proj, \(df) CBnet_ext(df, 20,
                                                 by = c("model", "ssp", "soilcode"))|>
                         calc_pbpsext(by = c("model", "ssp", "soilcode"),
                                   means = TRUE))
