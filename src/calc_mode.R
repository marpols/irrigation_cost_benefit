calc_mode <- function(x) {
  distinct_v <- unique(x)
  distinct_v[which.max(tabulate(match(x, distinct_v)))]
}