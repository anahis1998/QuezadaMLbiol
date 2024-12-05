#Overall pretty good, but you have too much code in main.R (typically that should
#just source each of your main analyses). Grade: S 

#DAN: You need a readme.md
#This code ran pretty easily, so that's good! 
#Too bad you had to go to overleaf, but I understand why.

##Workflows assignment 
#Name: Anahi Quezada
#Date: 11/03/2024

#Data: Air quality 
#link : https://archive.ics.uci.edu/dataset/360/air+quality


#This is the main code. 

#Setting my working directory: 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitWorkflows")
data <- read.csv("Data/AirQualityUCI1.csv")

#Libraries 
library(caret)
install.packages("RANN") #DAN: PLEASE don't use this. Make people install their own packages so they can
#decide if they have the disk space and want to do that. Auto-installing a package on someone's machine
#could be considered invasive, especially if they don't have the space. 
library(RANN)
library(dplyr)
library(ggplot2)

#Data preparation 
head(data)
dim(data)
data <- data[, 3:15] 
is.na(data)

#To omit NAs
source(file = "Code/na_delete.R")

data <- na_delete(data)
dim(data)
sum(is.na(data))
#To obtain the pollution column base on maximum level of concentration. 
umbral_Nox.GT <- 53
umbral_C6H6 <- 5
umbral_CO <- 6

data$contaminacion <- ifelse(data$CO.GT. > umbral_CO | 
                               data$C6H6.GT. > umbral_C6H6 | 
                               data$NOx.GT. > umbral_Nox.GT, 1, 0)
sum(is.na(data$contaminacion))

source(file = "Code/colores.R")
palette <- colores("blind_fr")
#Create the Results folder 

dir.create("Results")

png("Results/histograms.png", width = 1200, height = 400)
par(mfrow = c(1, 3))
hist(data$CO.GT., col=palette, main ="Histogram of CO concetration")
hist(data$C6H6.GT., col=palette, main= "Histogram of Benzene concentration")
hist(data$NOx.GT., col=palette, main="Histogram of Nitrogen")
dev.off()

#my contamination column is numeric, I need to transform to factor 
data$contaminacion <- factor(data$contaminacion, 
                                   levels = c(0, 1), 
                                   labels = c("L", "H"))
#L --> Low
#H -> High 
sum(is.na(data$contaminacion))
library(ggplot2)

# Create a bar plot with narrower bars and custom legend labels
png("Results/high_low.png", width = 300, height = 400)
ggplot(data, aes(x = contaminacion, fill = contaminacion)) +
  geom_bar(width = 0.5) +  
  scale_fill_manual(
    values = c("#35B779FF", "#482878FF" ) ,         
    name = "Contamination Level", 
    labels = c("Low", "High")     
  ) +
  labs(title = "Contamination Levels", x = "Presence of contamination", 
       y = "Count", font=3) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size=14, face= "bold"),
    axis.title.y = element_text(size=14, face = "bold")
  )
dev.off()

data1<-createDataPartition(data$contaminacion,
                           p=0.8,list=FALSE)

train <- data[data1,]
test <- data[-data1,] 

x_train <- train[, -ncol(train)]
y_train <- train$contaminacion


#ML Method
#Quinlan's C5.0 algorithm
modelLookup("C5.0")
traincontrol<-trainControl(method="repeatedcv",number=10,repeats=3)
m_C5p0<-train(x=x_train,
              y=y_train,
              method="C5.0",
              trControl=traincontrol,
              tuneLength=3,
              preProcess=c("center","scale"))

m_C5p0$results
max(m_C5p0$results$Accuracy) #-->0.9984924
print(m_C5p0$results)
table <- as.matrix(m_C5p0$results)

write.csv(m_C5p0$results, file = "Results/model_results.csv", row.names = FALSE)

#End of code 


