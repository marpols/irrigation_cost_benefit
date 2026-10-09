calc_SPEI <- function(climdf,
                      objs,
                      by = c("file_name","stncode", "ian","mo"),
                      joinby = "file_name",
                      latitudes,
                      scale = 2){

monthly <- climdf |>
  dplyr::summarise(Precip_mo = sum(Precipitation),
            Tmax_mo = mean(MaxTemp),
            Tmin_mo = mean(MinTemp),
            U2_mo = mean(WindSpeed),
            Rs_mo = mean(SolarRad),
            ed_mo = mean(VP),
            .by = by) |> 
  dplyr::inner_join(latitudes, by = joinby) |>
  data.table::as.data.table()


ETdf <- lapply(objs, function(o){
  df <- monthly[get(joinby) == o,] |>
    data.table::setnames(old =  c("ian", "mo"), new = c("YEAR", "MONTH"))
  
  df[,patm := 101.3][, PET :=
      SPEI::penman(Tmax = Tmax_mo,
             Tmin = Tmin_mo,
             lat = unique(latitude),
             P = patm,
             U2 = U2_mo,
             Rs = Rs_mo,
             ed = ed_mo/10)][
               ,BAL := Precip_mo - PET
             ]
  
}) |> setNames(objs)

spei <- lapply(ETdf, function(df){
  df <- df |> data.table::as.data.table()
  df[, spei :=
       SPEI::spei(df[,BAL],
            scale=scale,
       )[["fitted"]]]

}) |> purrr::list_rbind() |> data.table::as.data.table()
  
spei[, class := data.table::fcase(
  spei >=  2.0, "Extremely Wet",
  spei >=  1.5, "Very Wet",
  spei >=  1.0, "Moderate Wet",
  spei >=  -0.99, "Normal",
  spei >=  -1.49, "Moderate Dry",
  spei >=  -1.99, "Very Dry",
  spei < -1.99, "Extremely Dry",
  default = NA_character_
)]

}

