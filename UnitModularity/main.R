#DAN: Overall, nice work. Some good examples of modularity here. The code runs without
#too much hand-holding. I like your exploration of different ways to plot in color. 
#Grade: S+

#MODULARITY ASSIGNMENT
#Name: Anahi Quezada
#DATA: Precipitation and Temperature values from weather stations all over the USA
#This is the main code. 
#Setting my working directory: 

#DAN: Nice header! Maybe add a sentence on the research objective. 

#setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitModularity")
#DAN: Does not work on my machine! 

#MODULARITY EXAMPLE --> defining the workflow of my code
#Steps

#DAN: Good idea to plan your workflow

#1. Connect the new folder "UnitModularity" with my github online account. 
#2. Listing the Packages that I'm going to use. (to read the data, do plots, 
                                                 #colors, maps, etc)
#3. Checking on my data --> looking for NAs, checking the structures, rows, colums.
#4. Obtain the data for my calculations --> at least 40 measurements for each climate variable
#5. Calculate the slopes 
#6. Save the changes in my online repositorie for this assignment
#7. Make the histogram and save as a pdf
#8. Add the new 2 pdfs created
#9. Make the map and save as pdf
#10. Add the new file 
#11. Answer the question about "Climate Change"
#12. Do the write up and add to github. 
#13. Submit the assignment. 

#----Workflow 
#1. Connect to my github repositorie (check)
#2. Packages to use
install.packages("readRDS")
install.packages("maps") #DAN: I prefer not to install package in code because then when someone runs your code the 
#packages are installed on their machine though they may not have actually wanted that. Best, in  opinion,
#to indicate what they need and let them install. Avoids 
library(readRDS) #DAN: base R has a readRDS function
library(maps)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)

#3. Checking my data
#MODULARITY EXAMPLE -> define the variables instead of constants #DAN: OK, so do it!

precip <- readRDS(file = "USAAnnualPcpn1950_2008.rds")
temp <- readRDS(file="USAAnnualTemp1950_2008.rds")
dim(precip)
dim(temp)
#checking the structure of my data 
colnames(precip) #--> state, name, lon, lat, data and year
colnames(temp)   #--> state, name, lon, lat, data and year
#both of them, contain the same structure, then it will be easy to work. 

#My dataset's content
anyNA(precip) #--> TRUE
anyNA(temp)   #--> TRUE

#After checking that my data have NA values, I decided to create a function to 
#delete these values. 

#MODULARITY EXAMPLE -> na.omit 
#Application with Precipitation data
source(file = "na_delete.R")
pre_clean <- na_delete(precip)
dim(pre_clean)

#MODULARITY EXAMPLE -> Also, another option could be include the na.omit() into
#the process of obtain the 40 values at lest. I can do that here in my code 
#without the function, this way I'm splitting the process of 40 values at least 
#and delet the NAs values. 

#Application with Temperature data 
temp_clean <- na.omit(temp)
dim(temp_clean)
anyNA(temp_clean) #--> FALSE 

#4. My data to do the calculations

#At least 40 measurements 
#Precipitation

values <- 40 #Here, I show the value of the data requirement that I need, In 
            # this case, 40.  #DAN: Good
prec_40 <- pre_clean %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=values )%>%
  ungroup()

#Temperature --> the same way (at least 40 measurements)
temp_40 <- temp_clean %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=values )%>%
  ungroup()
#DAN: This is the same code twice. It's quick code thanks to tidyverse, but still, it repeats.
#So you missed an opportunity for modularity - could have written a function and called it twice. 

#5. Slopes 
#MODULARITY EXAMPLE ->  create the function to do the slopes and just call the 
#name of the fucntion here. This function can work for both of my data sets(vars)
source(file = "slopes_vars.R")
#DAN: Good modularity

#Precipitation
slopes_p <- prec_40 %>%
  group_by(state, name, lon, lat) %>%
  summarize(slope = slopes_vars(cur_data()))
print(slopes_p)

#Temperature
slopes_t <- temp_40 %>%
  group_by(state, name, lon, lat) %>%
  summarize(slope = slopes_vars(cur_data()))
print(slopes_t)

#6. Save changes and submit to github. 

#7. ---Histogram 
#MODULARITY EXAMPLE --> a function which display two options of palette of colors.
#I can develop more this idea with more options of colors. I like this idea because
#in my research I work with a lot of graphs and plots. 

#Details for the histogram 
#color blind friendly 
source(file = "colores.R")

pdf(file = "AQ_hist_temp.pdf")
title<-"Histogram of temperature slopes"
palette <- colores("blind_fr")
xlab <- "Temp slope" 
ylab <- "Frequency"
hist(slopes_t$slope,50, main= title, col =palette, xlab = xlab, ylab = ylab )
legend("topright", legend = "Note: this histogram contains
       blind friendly colors",  cex = 0.5)
dev.off()

pdf(file = "AQ_hist_precip.pdf")
title_1<-"Histogram of precipitation slopes"
palette_1 <- colores("non_blind_fr")
xlab_1 <- "Precip slope" 
ylab <- "Frequency"
hist(slopes_p$slope, 50, main= title_1, col =palette_1, xlab = xlab_1, ylab = ylab)
legend("topright", legend = "Note: this histogram contains
       non blind friendly colors",  cex = 0.5)
dev.off()

#8. Add the new 2 pdf to github (done)

#9. --MAPS

#To do the map I used ggplot2
plot1 <- ggplot(slopes_t,  aes(x = lon, y = lat, color = slope)) +
  borders("state") +  geom_point(size = 0.5) +
  scale_color_gradientn(colors = colores("blind_fr", n=10)) +  
  labs(title = "Map of temperature slopes", x = "Longitude", y = "Latitude", color = "slope") +
  theme_minimal()

map_1 <- map_data("state")
plot2 <- ggplot() +
  geom_polygon(data = map_1, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "white") +
  geom_point(data = slopes_p, aes(x = lon, y = lat, color = slope), size = 0.2) +
  scale_color_gradientn(colors = colores("non_blind_fr", n=10)) + 
  labs(title = "Map of Precipitation Slopes", color = "Slope (Precipitation)") +
  theme_minimal() +
  coord_fixed(1.3)

#10. Add the new file (done)

#11. Answer the question about "Climate Change"
#MODULARITY EXAMPLE --> answer the question : Climate change?
#To answer this question, I'm going to compare my results. 
#In order to do this, I'm going to present my results in different windows. 
#histograms...

x11()
pdf(file = "AQ_temp_precip.pdf")
par(mfrow = c(1, 2))
hist(slopes_t$slope,50, main= title, col =palette, xlab = xlab, ylab = ylab )
hist(slopes_p$slope, 50, main= title_1, col =palette_1, xlab = xlab_1, ylab = ylab)
dev.off()
#maps ... 
x11()
pdf(file = "AQ_temp_precip_maps.pdf")
grid.arrange(plot1, plot2, nrow = 2)
dev.off()
#12. Do the write up and add to github. 
#Can you see a warming trend with these data? Can you see a trend toward greater 
#or lesser precipitation over the period(1950-2008)?
#--> Temperature
#Speaking about the visual results coming from the maps and the histogram, I can 
#see there are more positive slopes, this can reflect that there is a clear trend
#to warm values, specifically this shows the climate change has been developing in 
#the 1950-2008 period. About specific examples where the data shows these warmer 
#slopes are the states" Montana, North Dakota, Minnesota and Arizona. This states 
#as it's shown in the map contain some yellow points that reflect the 0.10 
#temperature slopes.


#--> Precipitation
# The most repeated values here are in the range (0.00-0.1) precipitation slopes.
#To me, there is not a clear trend that can relate with climate change, but from 
#between 0.15 until 0.4, there is a strong decrease in the presence of precipitation. 
#As is presented in the "Map of Precipitation Slopes", the states that have more 
#than 40 measurements are Ohio, Tennessee, and Alabama. For these states and 
#considering the colors I can conclude that Tennessee has more negative values 
#of precipitation, this way this state would have experienced periods of drought. 
#And Alabama has some points in purple reflecting the abundance of precipitation. 

#-->Interpretation: 
#I can conclude that both variables present a change in the normal dynamics, 
#this led to the decision that in this period (1950-2008) we have witnessed climate 
#change, with specific examples such as an increase in temperature and others 
#with a lack of precipitation. At an environmental level, this can mean prolonged 
#periods of drought, which alter the seasons and the ecosystems and behavior of animals. 

#13. Submit the assignment. (done)


