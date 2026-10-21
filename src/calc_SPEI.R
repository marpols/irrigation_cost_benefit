calc_SPEI <- function(climdf,
                      objs,
                      by = c("stncode", "ian", "mo"),
                      joinby = "stncode",
                      latitudes,
                      scale = 2,
                      etfor = "Penman",
                      sims = character()) {
  monthly <- climdf |>
    dplyr::summarise(
      Precip_mo = sum(Precipitation),
      Tmax_mo = mean(MaxTemp),
      Tmin_mo = mean(MinTemp),
      U2_mo = mean(WindSpeed),
      Rs_mo = mean(SolarRad),
      ed_mo = mean(VP),
      .by = by
    ) |>
    dplyr::inner_join(latitudes, by = joinby) |>
    data.table::as.data.table()
  
  ETdf <- lapply(objs, function(o) {
    
    if (etfor == "Penman"){
    df <- monthly[get(joinby) == o, ] |>
      data.table::setnames(old =  c("ian", "mo"),
                           new = c("YEAR", "MONTH"))
    
    df[, patm := 101.3][, PET :=
                          SPEI::penman(
                            Tmax = Tmax_mo,
                            Tmin = Tmin_mo,
                            lat = unique(latitude),
                            P = patm,
                            U2 = U2_mo,
                            Rs = Rs_mo,
                            ed = ed_mo / 10
                          )][, BAL := Precip_mo - PET]
    
  } else if (etfor == "STICS"){
    
    sims <- data.table::as.data.table(sims)
    df <- sims[get(joinby) == o,] |>
      dplyr::summarise(
      PET = sum(etpp_n),
      .by = c("stncode", "soilcode", "ian", "mo")
    ) |>
      dplyr::inner_join(monthly, by = c(joinby, "ian", "mo")) |>
      data.table::setnames(old =  c("ian", "mo"),
                           new = c("YEAR", "MONTH")) |>
      data.table::as.data.table()
      
    
    df[,
       patm := 101.3
       ][,
      BAL := Precip_mo - PET
      ]
    
  }
  }) |> setNames(objs)
  
  spei <- lapply(ETdf, function(df) {
    df <- df |> data.table::as.data.table()
    df[, spei :=
         SPEI::spei(df[, BAL], scale = scale, )[["fitted"]]]
    
  }) |> purrr::list_rbind() |> data.table::as.data.table()
  
  spei[, spei_class := data.table::fcase(
    spei >=  2.0,
    "Extremely Wet",
    spei >=  1.5,
    "Very Wet",
    spei >=  1.0,
    "Moderate Wet",
    spei >=  -0.99,
    "Normal",
    spei >=  -1.49,
    "Moderate Dry",
    spei >=  -1.99,
    "Very Dry",
    spei < -1.99,
    "Extremely Dry",
    default = NA_character_
  )]
  
}
