stns <- c("S", "NG", "HCC", "EP")
soils <- c("CLO")
folder <- "C:\\Users\\marpo\\Documents\\Research\\Project - Irrigation Assessment\\Figures - 20261019\\PET"

lapply(stns, \(stn){
  ggplot2::ggplot(hist_SPEI[stncode == stn,], aes(x = PET)) +
    ggplot2::geom_density() +
    labs(title = "PET calculated w/ SPEI package",
         subtitle = stn)
  ggsave(file.path(folder, sprintf("SPEIpkg_%s.png", stn)))
  lapply(soils, \(soil){
    ggplot2::ggplot(hist_SPEI2[stncode == stn & soilcode == soil,],
                    aes(x = PET)) +
      ggplot2::geom_density() +
      labs(title = "PET (monthly means) calculated w/ STICS (etpp(n))",
           subtitle = sprintf("%s", stn))
    ggsave(file.path(folder, sprintf("STICSpet_%s.png", stn)))
    ggplot2::ggplot(hist_dynamic$noirr[stncode == stn & soilcode == soil,], aes(x = etpp_n)) +
      ggplot2::geom_density() +
      labs(title = "Daily PET calculated w/ STICS (etpp(n))",
           subtitle = sprintf("%s", stn))
    ggsave(file.path(folder, sprintf("STICSpet_%s_daily.png", stn)))
  })
})



hist_dynamic$noirr <- data.table::as.data.table(hist_dynamic$noirr)

