#Function to delete NAs

na_delete <- function(data){
  remove_na <- na.omit(data)
  return(remove_na)
}
#DAN: This is an unnecessary function because all it does is call na.omit. Any place where
#you would call this function you could instead just call na.omit!