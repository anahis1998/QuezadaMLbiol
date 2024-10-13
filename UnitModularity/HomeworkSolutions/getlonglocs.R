#MODULARITY EXAMPLE - Note that the comments at the top of the below function are detailed,
#and precisely specify the inputs and outputs of the function. This way the interface
#is clear between this module and whatever code chunk uses it.

#A function for filtering to only those locations where at least  some minimum number of 
#measurements were made over time.
#
#Args
#d        The data, intended to be one of the data frames df and dp above
#minpts   Minimum number of measurements required to include a location
#
#Output
#Same format as the input d, but with some of the locations removed (the ones with
#too few measurements taken)
#
getlonglocs<-function(d,minpts)
{
  #Create a data frame which holds the number of observations for each location where 
  #there was a weather station. MODULARITY EXAMPLE - This header and the one below are pseudocode
  #which I wrote before writing the actual code - top-down design promotes modularity
  ar<-aggregate(x=d$data,by=d[c('state','name','lat','lon')],FUN=function(x){sum(is.finite(x))})
  
  #Now keep only the rows of d which correspond to locations with enough samples
  goodlocs<-ar[ar$x>=minpts,c('name','state','lat','lon')] 
  d<-d[paste(d$name,d$state,d$lat,d$lon) %in% 
         paste(goodlocs$name,goodlocs$state,goodlocs$lat,goodlocs$lon),]
  
  #return the answer
  rownames(d)<-NULL
  return(d)
}
