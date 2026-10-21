pbpplot_hist <- function(df, pbcol){

dataset <- df |>
  dplyr::mutate(
    stncode = factor(stncode,
                      levels = c("S", "NG", "HCC", "EP")),
    system = factor(system,
                             levels = c("pivot I",
                                        "pivot II",
                                        "hose reel + sprinkler",
                                        "hose reel + boom cart")
    )
  )

ggplot(dataset, aes(stncode)) +
  geom_col(aes(y = .data[[pbcol]],
               fill = soilcode),
           position = "dodge") +
  geom_hline(yintercept = 20,
             linewidth = 0.5,
             colour = "red") +
  scale_fill_manual(values = c(
    "ARY" = "#33A02C",
    "CTW" = "#CAB2D6",
    "CLO" = "#FDBF6F")) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(title = "Payback Period",
       y = "Years",
       x = "weather station",
       fill = "Soil") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_grid(cols = vars(system))

}

pbpplot_proj <- function(df, pbcol){
  
  dataset <- df |>
    dplyr::mutate(
      ssp = factor(ssp,
                       levels = c("ssp126", "ssp370", "ssp585")),
      system = factor(system,
                      levels = c("pivot I",
                                 "pivot II",
                                 "hose reel + sprinkler",
                                 "hose reel + boom cart")
      )
    )
  
  ggplot(dataset, aes(ssp)) +
    geom_col(aes(y = .data[[pbcol]],
                 fill = soilcode),
             position = "dodge") +
    geom_hline(yintercept = 20,
               linewidth = 0.5,
               colour = "red") +
    scale_fill_manual(values = c(
      "ARY" = "#33A02C",
      "CTW" = "#CAB2D6",
      "CLO" = "#FDBF6F")) +
    coord_cartesian(ylim = c(0, 25)) +
    labs(title = "Payback Period",
         y = "Years",
         x = "ssp",
         fill = "Soil") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_grid(cols = vars(system))
  
}




