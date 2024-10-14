#MODULARITY ASSIGNMENT
#Name: Anahi Quezada
#DATA: Precipitation and Temperature values from weather stations all over the USA
#This is the main code. 
#Setting my working directory: 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitModularity")

#MODULARITY EXAMPLE --> defining the workflow of my code
#Steps

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

install.packages("readRDS")
library(readRDS)
library(maps)
library(dplyr)
library(ggplot2)
library(viridis)


#MODULARITY EXAMPLE -> define the variables instead of constants 

precip <- readRDS(file = "USAAnnualPcpn1950_2008.rds")
temp <- readRDS(file="USAAnnualTemp1950_2008.rds")
dim(precip)
dim(temp)
colnames(precip)
colnames(temp)

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

#At least 40 measurements 
#Precipitation

values <- 40 #Here, I show the value of the data requirement that I need, In 
            # this case, 40.  
prec_40 <- pre_clean %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=values )%>%
  ungroup()

#Temperature --> the same way (at least 40 measurements)
temp_40 <- temp_clean %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=values )%>%
  ungroup()

#MODULARITY EXAMPLE ->  create the function to do the slopes and just call the 
#name of the fucntion here. This function can work for both of my data sets(vars)
source(file = "slopes_vars.R")

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

#---Histogram 
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
title<-"Histogram of precipitation slopes"
palette <- colores("non_blind_fr")
xlab <- "Precip slope" 
ylab <- "Frequency"
hist(slopes_p$slope, 50, main= title, col =palette, xlab = xlab, ylab = ylab)
legend("topright", legend = "Note: this histogram contains
       non blind friendly colors",  cex = 0.5)
dev.off()

#--MAPS

#To do the map I used ggplot2
grad <- na.omit(slopes_2)
map_1 <- map_data("country")
ggplot() +
  geom_polygon(data = map_1, aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  geom_point(data = slopes_2, aes(x = lon, y = lat, color = slopes_t$slope), size = 0.2) +
  scale_color_gradient(low = "yellow", high = "blue", na.value = "red") +  
  labs(title = "Map of Temperature Slopes", color = "Slope (Precip)") +
  theme_minimal() +
  coord_fixed(1.3)
ggplot() +
  geom_polygon(data = map_1, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "white") +
  geom_point(data = slopes_2, aes(x = lon, y = lat, color = slopes_p$slope), size = 0.2) +
  scale_color_gradient(low = "blue", high = "red", na.value = "grey") + 
  labs(title = "Map of Precipitation Slopes", color = "Slope (Temperature)") +
  theme_minimal() +
  coord_fixed(1.3)


