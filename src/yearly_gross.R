yearly_gross <- function(df){
  df[
    , `:=`(
      gross_70 = 0.7 * yg * mp,
      gross_90 = 0.9 * yg * mp
    )
  ]
}
