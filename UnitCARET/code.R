#MULTIPLE METHODS ASSIGNMENT
#Name: Anahi Quezada
#DATA: Air quality

#This is the main code. 
#Setting my working directory: 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitCART/heart_disease/heart+disease")
data <- read.csv("AirQualityUCI1.csv")
heart <- read.table("processed.cleveland.data", header = F, sep = ",")
head(heart)
names(heart)<-c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
                "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
dim(heart)
graphics.off()

library(caret)
install.packages("RANN")
library(RANN)
#Data preparation 
head(data)
dim(data)

set.seed(101)
#data <- heart[1:15]
is.na(heart)
#C6H6 (GT) <- True hourly averaged Benzene concentration in microg/m^3 (reference analyzer)
#             microg/m^3
inds<-createDataPartition(heart$cp,p=0.8,list=FALSE)
#I have some missing values are tagged with -200, then I'm going to impute these. 
data_filtered <- data[rowSums(is.na(data)) != ncol(data), ]
data_filtered[data_filtered == -200] <- NA
data1 <- preProcess(x = data_filtered,method = "knnImpute", k = 5 )
all_missing_rows <- rowSums(is.na(data_partial)) == ncol(data_partial)
sum(all_missing_rows)
col_missing_counts <- colSums(is.na(data_partial))
col_missing_counts[col_missing_counts > 0]
str(data_partial)
numeric_data <- data_partial[sapply(data_partial, is.numeric)]
preProcess_model <- preProcess(numeric_data, method = "knnImpute", k = 5)
imputed_data <- predict(preProcess_model, numeric_data)
#
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
