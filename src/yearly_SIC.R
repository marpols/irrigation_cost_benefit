yearly_SIC <- function(df){
  syscosts <- irrcosts()
  
  for(sys in syscosts){
  col <- sprintf("%s_SIC", sys[["id"]])
  df[, (col) := SIC(df[["totir"]],sys[["operation"]])]
  }
  
  invisible(df)
}
