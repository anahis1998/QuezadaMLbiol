#A function for performing the regression of the climate variable against year for 
#each location, separately. 
#
#Args
#d        The data, intended to be one of the data frames df and dp above
#
#Output
#A data frame with columns state, name, lon, lat, slope, with one row for each
#location. The "slope" column has the regression slope of climate against year
#for that location.
#
getslopes<-function(d)
{
  #make a receptacle for the answers. MODULARITY EXAMPLE - Again the use of comments
  #which were originally pseudocode and were filled in with code below.
  res<-unique(d[,1:4])
  res$slope<-NA*numeric(dim(res)[1])
  
  #iterate through each row of res
  for (counter in 1:(dim(res)[1]))
  {
    #extract the rows of d which pertain to this row of res
    h<-d[d[[1]]==res[counter,1] &
           d[[2]]==res[counter,2] &
           d[[3]]==res[counter,3] &
           d[[4]]==res[counter,4],]
    
    #do the regression
    res[counter,"slope"]<-coef(lm(h$data~h$year))[2]
  }
  
  return(res)
}
