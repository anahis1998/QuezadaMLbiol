#Function to delete NAs

na_delete <- function(data){
  remove_na <- na.omit(data)
  return(remove_na)
}