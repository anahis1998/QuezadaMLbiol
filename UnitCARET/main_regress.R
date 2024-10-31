#MULTIPLE METHODS ASSIGNMENT
#Name: Anahi Quezada
#DATA: Air quality
#link : https://archive.ics.uci.edu/dataset/360/air+quality

#This is the main code for Regression. 

#Setting my working directory: 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitCARET")
data <- read.csv("AirQualityUCI1.csv")
graphics.off()

library(caret)
install.packages("RANN")
library(RANN)

#Data preparation 
head(data)
dim(data)
#I just need the contonous variables
data <- data[, c(6,13,14,15)] 
#To omit NAs
data <- na.omit(data)
 
formula <- C6H6.GT. ~ T + RH + AH
  
#I need as data frame 
data <- as.data.frame(data)

data$C6H6.GT. <- as.numeric(data$C6H6.GT.)
data$T <- as.numeric(data$T)
data$RH <- as.numeric(data$RH)
data$AH <- as.numeric(data$AH)

lapply(X=data,FUN=function(x){class(x)})

#Split the data
set.seed(101)
database<-createDataPartition(data$C6H6.GT., p = 0.8, list = FALSE)
train <- data[database,] 
test <- data[-database, ]
#independent variable 
xval <- train[, 2:4]
#dependent variable 
yval <- train[,1]

xtest <- test[, 2:4]
ytest <- test[ ,1]

pre_P <- preProcess(xval, method=c("center", "scale"))
xval <- predict(pre_P, xval)
xtest<- predict(pre_P, xval)

#Parallel Processing
library(doParallel)
cl <- makePSOCKcluster(5) 
registerDoParallel(cl)

#Train control 
train_c <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3)

#caret models 

#1 Lasso
library(quantregForest)

modelLookup("lasso")
model_lasso <- train(x = xval,
                     y = yval,
                     method = "lasso",
                     trControl = train_c,
                     tuneLength = 15,
                     preProcess = c("center","scale"))

min(model_lasso$results$RMSE)  #--> 7.27327   
max(model_lasso$results$Rsquared)   #--> 0.04398629
model_lasso$
#2 Conditional Inference Tree
install.packages("party")
library(party)
modelLookup("ctree")
m_ctree <- train(x = xval,
                 y = yval,
                 method = "ctree",
                 trControl = train_c,
                 tuneLength = 1,
                 preProcess = c("center","scale"))
m_ctree$results$RMSE #--> 7.237224
m_ctree$results$Rsquared #--> 0.5510207

#3 Gaussian Process model 
modelLookup("gaussprLinear")
model_gaussianp <- train(x = xval,
                         y = yval,
                         method = "gaussprLinear",
                         trControl = train_c,
                         tuneLength = 1,
                         preProcess = c("center","scale"))
model_gaussianp$results$RMSE #--> 7.286634
model_gaussianp$results$Rsquared #--> 0.04786163
  
#4 Bayesian Generalized Linear Model
modelLookup("bayesglm")
m_bayesglm <- train(x = xval,
                        y = yval,
                        method = "bayesglm",
                        trControl = train_c,
                        tuneLength = 1,
                        preProcess = c("center","scale"))

m_bayesglm$results$RMSE       #-->7.285862
m_bayesglm$results$Rsquared  #--> 0.04772132

#5 Partial Least Squares
modelLookup("kernelpls")
m_pls <- train(x = xval,
                   y = yval,
                   method = "kernelpls",
                   trControl = train_c,
                   tuneLength = 15,
                   preProcess = c("center","scale"))

min(m_pls$results$RMSE)      #--> 7.293804
max(m_pls$results$Rsquared)  #--> 0.04564233

#6 Quantile Random Forest
library(quantregForest)
modelLookup("qrf")
m_qrf <- train(x = xval,
                   y = yval,
                   method = "qrf",
                   trControl = train_c,
                   tuneLength = 15,
                   preProcess = c("center","scale"))

min(m_qrf$results$RMSE)   #--> 7.861478   
max(m_qrf$results$Rsquared) #--> 0.04247999

#7 Bagged MARS
library(earth)
modelLookup("bagEarth")
m_bagearth <-  train(x = xval,
                     y = yval,
                     method = "bagEarth",
                     trControl = train_c,
                     tuneLength = 1,
                     preProcess = c("center","scale"))

m_bagearth$results$RMSE   #-->7.247809
m_bagearth$results$Rsquared   #--> 0.5130481

#Models in a loop 
models_l <- c("lasso",
              "ctree",
              "gaussprLinear",
              "bayesglm",
              "kernelpls",
              "qrf",
              "bagEarth")

t_l <- sapply(as.list(models_l), FUN=function(x){dim(modelLookup(x))[1]})
t_l <- round(25^(1/t_l))

#Results
m_result <- list()
acc_values <-data.frame(model = models_l,
                             RMSE = NA*numeric(length(models_l)),
                             Rsquared = NA*numeric(length(models_l)),
                             MSE = NA*numeric(length(models_l)),
                             RMSESD = NA*numeric(length(models_l)),
                             RsquaredSD = NA*numeric(length(models_l)),
                             MAESD = NA*numeric(length(models_l)))


for (counter in 1:length(models_l)) {
  print(paste0(models_l[counter],"; ",counter," of ",length(models_l)))
  m_result[[counter]] <- train(x = xval,
                                   y = yval,
                                   method = models_l[counter],
                                   trControl = train_c,
                                   tuneLength = t_l[counter]) 
  ind <- which(m_result[[counter]]$results$RMSE == max(m_result[[counter]]$results$RMSE))
  ind <- ind[1]
  acc_values[counter,2:7] <- m_result[[counter]]$results[ind,c("RMSE",
                                                                        "Rsquared",
                                                                        "MAE",
                                                                        "RMSESD",
                                                                        "RsquaredSD",
                                                                        "MAESD")]
}
acc_values
#Results 
#Best model 
#Bagged MARS with the highest RMSE and the lowest Rsquared. 
#No overfitting across my data. 
