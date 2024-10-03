#CART ASSINGMENT 
#Name: Anahi Quezada
#Data: heart disease
#Source: UC Irvine - Machine Learning Repository 
#link: "https://archive.ics.uci.edu/dataset/45/heart+disease"

#Index
#1. Libraries
#2. Data
#3. Models
#3.1 Pre-pruned model
#3.2 Full CART
#3.3 Pruned CART
#3.4 Bagging model
#3.5 Random forest model
#4 Best model with 25% data

#1. ----LIBRARIES
rm(list=ls())
graphics.off()
library(rpart)
library(ipred)
library(randomForest)
library(adabag)
library(xgboost)

#2. ----DATA
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitCART/own_data/heart+disease")
heart <- read.table("processed.cleveland.data", header = F, sep = ",")
head(heart)
names(heart)<-c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
                "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
dim(heart)
#I selected the "num" column because this is the predicted attribute
#Additional information about this column:
#num: diagnosis of heart disease (angiographic disease status)
#-- Value 0: < 50% diameter narrowing
#-- Value 1: > 50% diameter narrowing
unique(heart$num)
heart$num<-as.factor(heart$num)
head(heart)
#**split the data into validation and testing sets
#split
set.seed(10)#I change the set.seed value because I was having errors later. 
d_perm<-heart[sample(dim(heart)[1],dim(heart)[1],replace=FALSE),]
d_val<-d_perm[1:floor(.75*(dim(heart)[1])),]
unique(d_val$thal)
d_val$thal <- as.factor(ifelse(d_val$thal == "?", NA, d_val$thal))
#issues with NA in my data 
moda_thal <- names(sort(table(d_val$thal), decreasing = TRUE))[1]
d_val$thal[is.na(d_val$thal)] <- moda_thal
#checking
unique(d_val$thal)

#issues with ca 
d_val$ca[d_val$ca == "?"] <- NA
moda_ca <- names(sort(table(d_val$ca), decreasing = TRUE))[1]
d_val$ca[is.na(d_val$ca)] <- moda_ca
levels(d_val$ca)
unique(d_val$ca)

d_test<-d_perm[(floor(.75*(dim(heart)[1]))+1):(dim(heart)[1]),]
unique(d_test$thal)
#ca issues ?
d_test$ca[d_test$ca == "?"] <- NA
moda_ca1 <- names(sort(table(d_test$ca), decreasing = TRUE))[1]
d_test$ca[is.na(d_test$ca)] <- moda_ca1
unique(d_test$ca)

#some consistency checks
dim(heart)
dim(d_perm)
dim(d_val)
dim(d_test)
dim(d_val)[1]+dim(d_test)[1]

#3.------ MODELS
#3.1 ------------Pre-pruned model
m_d<-rpart(num~.,data=d_val,method="class")

#examine it
m_d #pretty simple
plot(m_d,uniform=TRUE,margin=0.1)
text(m_d,use.n=TRUE,all=TRUE,cex=0.8) 

#save as pdf
pdf(file="heartDisease1.pdf")
plot(m_d,uniform=TRUE,margin=0.1)
text(m_d,use.n=TRUE,all=TRUE,cex=2) 
dev.off()

#see the errors it makes on the training data
m_d_pred<-predict(m_d,type="class")
table(m_d_pred,d_val$num)
sum(m_d_pred!=d_val$num)/dim(d_val)[1] #This is the value that I need
                                      #0.2907489  
#3.2------------FULL CART

#Important details:
#cp=0 
#minsplit=1 
m_f<-rpart(num~.,data=d_val,method="class",
           control=rpart.control(cp=0,minsplit=1)) 
#examine it
m_f
plot(m_f,uniform=TRUE,margin=0.1)
text(m_f,use.n=TRUE,all=TRUE,cex=0.8) 

#see the errors it makes on the training data
m_f_pred<-predict(m_f,type="class")
table(m_f_pred,d_val$num) 
numgp<-10 #number of folds in k-fold cross validation
gp<-rep(1:numgp,length.out=dim(d_val)[1])
xerrs_d<-NA*numeric(numgp)
xerrs_f<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fiting predictions
  #full model
  m_f_s<-rpart(num~.,
               data=d_val[gp!=counter,],method="class",control=rpart.control(cp=0,minsplit=1))
  #Pre-pruned model
  m_d_s<-rpart(num~.,
               data=d_val[gp!=counter,],method="class")
  
  #get predictions for the left out group and get error rates
  pred_f<-predict(m_f_s,d_val[gp==counter,],type="class")
  pred_d<-predict(m_d_s,d_val[gp==counter,],type="class")
  xerrs_f[counter]<-sum(pred_f!=d_val$num[gp==counter])/sum(gp==counter)
  xerrs_d[counter]<-sum(pred_d!=d_val$num[gp==counter])/sum(gp==counter)
}
mean(xerrs_f) #0.4442688

mean(xerrs_d) #0.4357708

#3.3 ------------Pruned CART
plotcp(m_f) 
printcp(m_f) #after checking the graf, the value is 0.045, BEST CP

#save a plot for the slide show
pdf(file="ComplexityParameter_Full.pdf")
plotcp(m_f)
dev.off()

plotcp(m_d) 
printcp(m_d) #after cheching the graf, the value is 0.044
pdf(file="ComplexityParameter_pre_pruned.pdf")
plotcp(m_d)
dev.off()

#my best cp --> cp coming from Full Cart=0.045
m_f_5<-rpart(num~.,
           data=d_val,method="class",control=rpart.control(cp=0.027,minsplit=1))
m_f_5
m_f
#plots
plot(m_f_5,uniform=TRUE,margin=0.1) 
text(m_f_5,use.n=TRUE,all=TRUE,cex=0.8) 

x11()
plotcp(m_f_5)
plotcp(m_f) 

xerrs_5<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  m_5_s<-rpart(num~.,
               data=d_val[gp!=counter,],method="class",control=rpart.control(cp=0.027,minsplit=1))

  #get predictions for the left-out group and get error rates
  pred_5<-predict(m_5_s,d_val[gp==counter,],type="class")
  xerrs_5[counter]<-sum(pred_5!=d_val$num[gp==counter])/sum(gp==counter)
}
#Time to check the means
mean(xerrs_f) #--> 0.4442688
mean(xerrs_d) #--> 0.4357708
mean(xerrs_5) #--> 0.3693676 So this new model beats both m_d and m_f

#3.4 --------------Bagging model
#**now we do bagging and see if we can beat m_f_5

bagres <- bagging(num ~ ., 
                  data = d_val, 
                  nbagg = 500, 
                  control = rpart.control(cp = 0, minsplit = 1, xval = 0), 
                  aggregation = "majority")

predictions <- predict(bagres, newdata = d_val, type="class")
misclass_error <- mean(predictions != d_val$num)
print(misclass_error)#--> 0.9162996

b_pred <- predict(bagres, newdata = d_val, type = "class", aggregation = "majority")
sum(b_pred!=d_val$num)/dim(d_val)[1] #The same value --> 0.9162996

#**let's do x-val on the bagging result, in a comparable way to the manual x-val done above

xerrs_b<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  bagres_s<-bagging(num~., #again, the ipred:: prefix is just to make sure we are using the bagging routine from the ipred package
                    data=d_val[gp!=counter,],nbagg=500,coob=FALSE,method="class",
                    na.action = na.omit,
                    control=rpart.control(cp=0, minsplit=1,xval=0),aggregation="majority")
  
 
  predictions <- predict(bagres_s, newdata = d_val)
  misclass_error <- mean(predictions != d_val$num)
  print(misclass_error)
      
  
  #get predictions for the left out group and get error rates
  pred_b_s<-predict(bagres_s,d_val[gp==counter,],type="class")#,aggregation="majority")
  xerrs_b[counter]<-sum(pred_b_s!=d_val$num[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)#--> 0.4442688
mean(xerrs_d)#--> 0.4357708
mean(xerrs_5)#--> 0.3693676 *best value
mean(xerrs_b)#--> 1


#3.5 --------------Random forest model

m_rf<-randomForest(num~.,data=d_val,ntree=1000)

m_rf #includes oob error rate--> 37.44%
names(m_rf) 
tail(m_rf$err.rate) #last entry of column 1 is the same as the oob error rate above

plot(m_rf) #You can also see as the number of trees increases the accuracy increases and then
#levels off 
dim(m_rf$err.rate)[1] #corresponds to the number of trees=1000 trees

rf_pred<-predict(m_rf)
sum(m_rf$predicted==rf_pred)
length(rf_pred) #ok so predict is just pulling a slot that already is in m_rf
sum(rf_pred!=d_val$num)/dim(d_val)[1] #agrees with the oob error rate above --> 0.3744493

#**let's do x-val on the rf result, in a comparable way to the manual x-val done above

xerrs_rf<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  rfres_s<-randomForest(num~.,
                        data=d_val[gp!=counter,],ntree=1000)
  
  #get predictions for the left out group and get error rates
  pred_rf_s<-predict(rfres_s,d_val[gp==counter,],type="class")
  xerrs_rf[counter]<-sum(pred_rf_s!=d_val$num[gp==counter])/sum(gp==counter)
}


mean(xerrs_f)#--> 0.4442688
mean(xerrs_d)#--> 0.4357708
mean(xerrs_5)#--> 0.3693676 *best value
mean(xerrs_b)#--> 1
mean(xerrs_rf)# --> 1
#Comparable to bagging. Technically the best model (by a tiny bit) is the Pruned cart

#**Now apply adaptive boosting

m_ada <-boosting(num~.,
                     data = d_val,
                     mfinal = 10,
                     control = rpart.control(cp = 0, minsplit = 1, xval = 0))
d_val$age
ada_pred <- predict(m_ada, newdata = d_val)$class
sum(ada_pred!=d_val$num)/dim(d_val)[1] 

m_ada$importance #gives the relative importance of the different variables for
#making the final model predictions

#**let's see how adaptive boosting does on Xval

xerrs_ada<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  print(paste(counter,"of",numgp))
  
  #fit the models on all of the data excluding one group
  adares_s<-boosting(num~.,data=d_val[gp!=counter,])
  
  #get predictions for the left out group and get error rates
  pred_ada_s<-predict(adares_s,d_val[gp==counter,],type="class")$class
  xerrs_ada[counter]<-sum(pred_ada_s!=d_val$num[gp==counter])/sum(gp==counter)
}


mean(xerrs_f)#--> 0.4442688
mean(xerrs_d)#--> 0.4357708
mean(xerrs_5)#--> 0.3693676 *best value
mean(xerrs_b)#--> 1
mean(xerrs_rf)# --> 1
mean(xerrs_ada) #--> NA

#**Now apply EXTREME GRADIENT BOOSTING

#put the data in the specific form expected by the xgb function
X_val<-as.matrix(d_val[,2:(dim(d_val)[2])])
y_val<-as.integer(d_val[,1])-1

sapply(d_val, class)
d_val[] <- lapply(d_val, function(x) {
  if (is.character(x)) as.numeric(x) else x
})
X_val <- as.matrix(d_val[, 2:(dim(d_val)[2])])  # Usa todas las columnas excepto 'num'
y_val <- as.integer(d_val[, 1]) - 1              # Asegúrate de que 'num' sea el índice correcto
class(X_val)
d_val$ca <- as.numeric(as.character(d_val$ca))
d_val$thal <- as.numeric(as.character(d_val$thal))
X_val <- as.matrix(d_val[, 2:(dim(d_val)[2])])  # Asegúrate de que ahora todo sea numérico
y_val <- as.integer(d_val[, 1]) - 1              # Asegúrate de que 'num' sea el índice correcto
sapply(X_val, class)  # Debería mostrar "numeric" para todas las columnas
sapply(d_val, class)
d_val[] <- lapply(d_val, function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))  # Convierte factores a numéricos
  } else if (is.character(x)) {
    as.numeric(x)  # Convierte caracteres a numéricos
  } else {
    x  # Mantiene el tipo original si no es factor ni carácter
  }
})
X_val <- as.matrix(d_val[, 2:(dim(d_val)[2])])  # Asegúrate de que ahora todo sea numérico
y_val <- as.integer(d_val[, 1]) - 1              # Asegúrate de que 'num' sea el índice correcto
sapply(X_val, class)  # Debería mostrar "numeric" para todas las columnas
y_val <- ifelse(d_val$num >= 2, 1, 0)  # 1 para "enfermo", 0 para "sano"
unique(y_val)  # Verifica los valores únicos en y_val


m_xgb<-xgboost(data=X_val,
           label=y_val,
           max_depth=6,eta=.3,
           nthread=2,nrounds=20,
           objective="binary:logistic",verbose=2)

#get error rate on the validation data
pred_xgb<-predict(m_xgb,X_val) #gives probabilities 
#of being in the first class, which is additionally useful, tho 
#we don't use it
predictions<-rep(0,length(pred_xgb))
predictions[pred_xgb>.5]<-"1"
sum(predictions!=as.character(d_val$num))/dim(d_val)[1] #--> 0.4229075

#**Now do X val error rate for extreme gradient boosting

xerrs_xgb<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #status
  print(paste(counter,"of",numgp))
  
  #fit the models on all of the data excluding one group
  m_xgb_s<-xgboost(data=X_val[gp!=counter,],
                 label=y_val[gp!=counter],
                 max_depth=7,eta=.2,subsample=0.5,nrounds=55,
                 nthread=2,objective="binary:logistic",verbose=0)
  
  #get predictions for the left out group and get error rates
  pred_xgb_s<-predict(m_xgb_s,X_val[gp==counter,])
  predictions_s<-rep(0,length(pred_xgb_s))
  predictions_s[pred_xgb_s>.5]<-"1"
  xerrs_xgb[counter]<-sum(predictions_s!=
                            d_val$diagnosis[gp==counter])/sum(gp==counter)
}

mean(xerrs_f)#--> 0.4442688
mean(xerrs_d)#--> 0.4357708
mean(xerrs_5)#--> 0.3693676 *best value
mean(xerrs_b)#--> 1
mean(xerrs_rf)# --> 1
mean(xerrs_ada) #--> NA
mean(xerrs_xgb)#--> 0

importance_matrix<-xgb.importance(model=m_xgb)
importance_matrix 
#4 ----------------Best model with 25% data

#**Now apply THAT ONE MODEL ONLY on the test data and see prediction error
#--Pruned CART
testpred_ada<-predict(m_f_5,d_test[,1:14],type="class")#$class
sum(testpred_ada!=d_test$num)/dim(d_test)[1] #--> 0.5526316
#RESULTS
#By using this heart disease data with the predictive attribute (num) that 
#refers to the type of chest pain (values from 1 to 4, where 1: typical angina, 
#2: atypical angina, 3: non-anginal pain, 4: asymptomatic). 
#The model I obtained that best fits the data and has the lowest cross-validation 
#error is "Pruned cart" with a value of 0.3693676. However, when presenting this 
#model with the d_test, i.e. 25% of my original data, it presents a 
#cross-validation error of 0.5526316. With this result, I can make two 
#observations, first is that more work is needed with the variables 
#"ca" (number of main vessels (0-3) colored by fluoroscopy) and 
#"thal" (3 = normal; 6 = fixed defect; 7 = reversible defect), since these were 
#the ones that presented the most problems throughout my code due to their mix of
#"character", "missing value", "factor" and "number" format. Given this problem, 
#the following models are shown to be incomplete (values such as 1, 0 or NA).
#The second observation is that the "Pruned cart" model definitely does not 
#satisfy the correct definition of a "Classification and Regression Tree".
#Since, the both cross validation values are not similar or close enough. 
#With these results, I can assume my models have overfitting being that 
#"Prunned cart" do not work well for the d_test as it worked for the d_val. 
