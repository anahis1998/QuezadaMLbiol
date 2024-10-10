setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitModularity")
#Packages
install.packages("readRDS")
library(readRDS)
library(maps)
library(dplyr)
#Data
precip <- readRDS(file = "USAAnnualPcpn1950_2008.rds")
temp <- readRDS(file="USAAnnualTemp1950_2008.rds")

#Seperately for each of the two climate variables, and for each location, 
#separetely, for which there are at least 40 measurements, regress the climate
#variable against year and take the slope. Make a histogram of all the slopes,
#for each climate variable. Make a map for each climate variable where the slopes are
#depicted in color. The “maps” package may come in handy. Based on these results, 
#can you see climate change? What is happening with precipitation?
d_prec <- as.numeric(precip$year)
#at least 40 
prec_40 <- precip %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=40)

#function 
slope_p <- function(df, data){
  model <- lm(df[[data]] ~ year, precip=df)
  return(coef(model)["year"])
}
slopes_p <- slope_p %>%
  group_by(state, name, lon, lat) %>%
  summarize(
    slope_p_1 = get_slope(cur_data(), "data")
  )

hist(precip$year)
 
 