
##Workflows assigment 
#Name: Anahi Quezada
#Date: 11/03/2024

#Data: Scalesia pedunculata from Galapagos, Ecuador 

#Libraries 
library(rgbif)
library(dplyr)
library(ggplot2)
#Structure 

#1. Obtain the data 
#Data downloaded from Gbif with the next script 
#llamar al codigo de download 

#2. Clean the data with the next script 

#3. Plot the data just to check the previous clean steps were applied correctly 
write.csv(f, "scalesia1.csv", row.names=FALSE)

head(d)
str(d)
ggplot(f, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point() +
  labs(title = "Scalesia pedunculata Occurrences",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
#3. Define the ML method to evaluate the data 

#4. Create results folder 
#