#slopes function
slopes_vars <- function(df){
  df <- na.omit(df)
  if (nrow(df)<2){
    return(NA)
  }
  model <- lm(data ~ year, data = df)
  return(coef(model)["year"])
}