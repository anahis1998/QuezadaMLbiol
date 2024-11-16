#DAN: In the writeup, you were supposed to read about a couple of the methods which were new to you and then
#describe how they work and guidelines for using them - that appears to have been skipped.
#Good, well organized code. Good comments. You might have considered using a loop instead of 
#lots of repeats of similar code chunks.
#Looks like there are some conflicts remaining in the other doc.
#You need to use the data, code, results, paper format, but did not.
#Overall, decent job on the elements you focused on, but lots missing.
#Grade: S

#MULTIPLE METHODS ASSIGNMENT
#Name: Anahi Quezada
#DATA: Air quality
#link : https://archive.ics.uci.edu/dataset/360/air+quality

#This is the main code for classification. 

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

#The doParallel Package
library(doParallel)
clust <- makePSOCKcluster(5)
registerDoParallel(clust)

#Information on the model 
modelLookup("rpart")
#Train Control
traincontrol<-trainControl(method="repeatedcv",number=10,repeats=3)

m_rpart<-train(x=x_train,
               y=y_train,
               method="rpart",
               trControl=traincontrol)
class(m_rpart)
names(m_rpart)
m_rpart
m_rpart$results

prediction <- predict(m_rpart, train)
table(prediction, y_train)

predic_model <- predict(m_rpart$finalModel, train)
predic_model_1 <- apply(X=predic_model,MARGIN=1,
FUN=function(x){ifelse(x[1]<.5,"M","B")})
sum(prediction==predic_model_1)

#tuneLength    
m_rpart<-train(x=x_train,
               y=y_train,
               method="rpart",
               trControl=traincontrol,
               tuneLength=15) 
m_rpart$results
plot(m_rpart$results$cp, 1-m_rpart$results$Accuracy, 
     type="l")

#tuneGrid
tg <- data.frame(cp=seq(from=0.01, to=0.8, by=0.01))
m_rpart<-train(x=x_train,y=y_train,
               method="rpart",
               trControl=traincontrol,
               tuneGrid=tg) 
m_rpart$results
plot(m_rpart$results$cp,1-m_rpart$results$Accuracy,
     type="l")
max(m_rpart$results$Accuracy)#--> 0.9984924

#experiemntal 
set.seed(123)
m_rpart1<-train(x=x_train,
                y=y_train,
                method="rpart",
                trControl=traincontrol,
                tuneLength=15) 
set.seed(123)
m_rpart2<-train(x=x_train,
                y=y_train,
                method="rpart",
                trControl=traincontrol,
                tuneLength=15,
                preProcess=c("center","scale"))

cbind(m_rpart1$results$Accuracy,m_rpart2$results$Accuracy)
m_rpart1$results$Accuracy-m_rpart2$results$Accuracy 
#[7.538067e-06  7.538067e-06  7.538067e-06  7.538067e-06  7.538067e-06
#7.538067e-06  7.538067e-06  7.538067e-06  7.538067e-06  7.538067e-06
#7.538067e-06  7.538067e-06  7.538067e-06  7.538067e-06 -4.522840e-05

#Models 
#1k-nearest neighbors
modelLookup("knn")
model_knn<-train(x=x_train,
                 y=y_train,
                 method="knn",
             trControl=traincontrol,
             tuneLength=15,
             preProcess=c("center","scale"))

model_knn$results
head(model_knn$results)
max(model_knn$results$Accuracy)#--> 0.9622343

#2 Quinlan's C5.0 algorithm
modelLookup("C5.0")
m_C5p0<-train(x=x_train,
              y=y_train,
              method="C5.0",
              trControl=traincontrol,
              tuneLength=3,
              preProcess=c("center","scale"))

m_C5p0$results
max(m_C5p0$results$Accuracy) #-->0.9984999

#3 Support vector machine
install.packages("kernlab")
library(kernlab) 

modelLookup("svmRadial")
m_svm <- train(x = x_train,
                    y = y_train,
                    method = "svmRadial",
                    trControl = traincontrol,
                    tuneLength = 4,
                    preProcess = c("center","scale"))

max(m_svm$results$Accuracy)  #--> 0.9712573

#3 Learning vector quantization 
library(class)
modelLookup("lvq")
m_lvq <-  train( x= x_train,
                     y = y_train,
                     method = "lvq",
                     trControl = traincontrol,
                     tuneLength = 1)

max(m_lvq$results$Accuracy) #---> 0.9400799

#4 Boosted Tree
install.packages("party")
install.packages("mboost")
install.packages("plyr")
install.packages("partykit")

library(party) 
library(partykit)
library(plyr)
library(mboost)

modelLookup("blackboost")
m_boostedt <-  train( x= x_train,
                             y = y_train,
                             method = "blackboost",
                             trControl = traincontrol,
                             tuneLength = 5,
                             preProcess = c("center","scale"))

max(m_boostedt$results$Accuracy) #--> 0.9959671
#5 Multivariate Adaptive Regression Spline
install.packages("earth")
library(earth)
modelLookup("earth")
m_earth <-  train( x= x_train,
                   y = y_train,
                   method = "earth",
                   trControl = traincontrol,
                   tuneLength = 2)

max(m_earth$results$Accuracy) #--> 0.9934645
#Now try, old models 
#1 Random Forest
library(randomForest)
modelLookup("rf")
m_randomF <- train( x= x_train,
                    y = y_train,
                    method = "rf",
                    trControl = traincontrol,
                    tuneLength = 5,
                    preProcess = c("center","scale"))
max(m_randomF$results$Accuracy) # --> 0.9984848

#2 Adaptive Boosting 
library(adabag)
modelLookup("AdaBoost.M1")
m_adaB <- train(x = x_train,
                        y = y_train,
                        method = "AdaBoost.M1",
                        trControl = traincontrol,
                        tuneLength = 3,
                        preProcess = c("center","scale"))
max(m_adaB$results$Accuracy) #--> 0.9984924

#3 Bagging Model 
library(plyr)
library(ipred)
library(e1071)
modelLookup("treebag")
m_bagging <- train(x = x_train,
                       y = y_train,
                       method = "treebag",
                       trControl = traincontrol,
                       tuneLength = 5,
                       preProcess = c("center","scale"))

max(m_bagging$results$Accuracy) #--> 0.9984924

#loop 
traincontrol <- trainControl(method="repeatedcv", 
                             number=10, repeats=3)

names_model <- c("rpart",
                 "knn",
                 "C5.0", 
                 "svmRadial",
                 "lvq",
                 "blackboost",
                 "earth",
                 "rf",
                 "AdaBoost.M1", "treebag")
                
tLook <-c(15, 15, 3,4,1,5,2,5,3,5)
mode <- list()
acc1<-data.frame(model=names_model,
                Accuracy=NA*numeric(length(names_model)),
                Kappa=NA*numeric(length(names_model)),
                AccuracySD=NA*numeric(length(names_model)),
                KappaSD=NA*numeric(length(names_model)))

for (counter in 1:length(names_model))
{
  print(paste0(names_model[counter],"; ",counter," of ",length(names_model)))
  mode[[counter]]<-train(x=x_train,y=y_train,method=names_model[counter],
                           trControl=traincontrol,tuneLength = tLook[counter],
                         preProcess=c("center","scale")) 
  ind<-which(mode[[counter]]$results$Accuracy==max(mode[[counter]]$results$Accuracy))
  ind<-ind[1]
  acc1[counter,2:5]<-mode[[counter]]$results[ind,c("Accuracy","Kappa","AccuracySD",
                                                   "KappaSD")]
}
acc1

#My best model according to the highest accuracy is
#Random Forest --> Accuracy=0.9984999

#Now, I'm going to use the testing data 
#test <- data_clean[-data1] 

pred_test <- predict(m_randomF, 
              newdata= test)
confusionMatrix(pred_test, test$contaminacion) 

####---  Confusion Matrix 
#Confusion Matrix and Statistics

#Reference
#Prediction   Low   High
         #L   17    0
        #H    0    148

#Accuracy : 1          
#95% CI : (0.9779, 1)
#No Information Rate : 0.897      
#P-Value [Acc > NIR] : 1.616e-08  

#Kappa : 1          

#Mcnemar's Test P-Value : NA         
                                     
 #           Sensitivity : 1.000      
  #          Specificity : 1.000      
   #      Pos Pred Value : 1.000      
      #   Neg Pred Value : 1.000      
       #      Prevalence : 0.103      
        # Detection Rate : 0.103      
   #Detection Prevalence : 0.103      
    #  Balanced Accuracy : 1.000      
                                     
     #  'Positive' Class : L

save.image(file="CaretClassf.RData")

stopCluster(cl)
#Results 
#About the old models that I've applied to my data, the best model was
#random forest (Accuracy=0.9984999). On the other hand, with the new models 
#applied, the best one was --> Quinlan's C5.0 algorithm with 0.9984848 accuracy. 

#After this process, I applied random forest to my test data and the results suggest 
#that even though the model aligns perfect with my data (0.9779% of accuracy) and 
#a good p-value, maybe "random forest model" is very over fitted to my data.
#This way, my test data is not working properly. 
