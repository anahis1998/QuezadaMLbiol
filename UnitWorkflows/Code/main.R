
##Workflows assigment 
#Name: Anahi Quezada
#Date: 11/03/2024

#Data: Air quality 
#link : https://archive.ics.uci.edu/dataset/360/air+quality


#This is the main code. 

#Setting my working directory: 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitCARET")
data <- read.csv("AirQualityUCI1.csv")

#Libraries 
library(caret)
install.packages("RANN")
library(RANN)
library(rgbif)
library(dplyr)
library(ggplot2)

#Data preparation 
head(data)
dim(data)
data <- data[, 3:15] 
is.na(data)

#To obtain the pollution column base on maximum level of concentration. 
umbral_Nox.GT <- 53
umbral_C6H6 <- 5
umbral_CO <- 6
data$contaminacion <- ifelse(data$CO.GT. > umbral_CO | 
                               data$C6H6.GT. > umbral_C6H6 | 
                               data$NOx.GT. > umbral_Nox.GT, 1, 0)

#To omit NAs
data_clean <- na.omit(data)

#my contamination column is numeric, I need to transform to factor 
data_clean$contaminacion <- factor(data_clean$contaminacion, 
                                   levels = c(0, 1), 
                                   labels = c("L", "H"))
#L --> Low
#H -> High 

data1<-createDataPartition(data_clean$contaminacion,
                           p=0.8,list=FALSE)

train <- data_clean[data1,]
test <- data_clean[-data1,] 

x_train <- train[, -ncol(train)]
y_train <- train$contaminacion


#ML Method
#Quinlan's C5.0 algorithm
modelLookup("C5.0")
m_C5p0<-train(x=x_train,
              y=y_train,
              method="C5.0",
              trControl=traincontrol,
              tuneLength=3,
              preProcess=c("center","scale"))

m_C5p0$results
max(m_C5p0$results$Accuracy) #-->0.9984999




#2. Clean the data with the next script 

#3. Plot the data just to check the previous clean steps were applied correctly 

#3. Define the ML method to evaluate the data 

#4. Create results folder 
#