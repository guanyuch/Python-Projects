# 2.gradientboost:
# load data
library(caret)

airbnb<-read.csv('C:/Users/Xin Gu/Desktop/BA courses/758T/project/Airbnb/fifth/Train_XY_AllNumeric_4.26.csv')

# Set the seed to create reproducible train and test sets
set.seed(1234)

# Create a stratified random sample to create train and test sets
# Reference the outcome variable
trainIndex   <- createDataPartition(airbnb$high_booking_rate, p=0.8, list=FALSE, times=1)

train        <- airbnb[ trainIndex, ]
test         <- airbnb[-trainIndex, ]

X_train=train[,-1]
y_train=train[,1]

X_test=test[,-1]
y_test=test[,1]

# Load the gradientboost package
library(gbm)

##train gbm model default:
GBM <- train(high_booking_rate ~., data=train, 
             method="gbm")
# Create our prediction probabilities
preds<-predict(GBM,X_test)

#use cutoff=0.5 to classify it
pred<-ifelse(preds >=0.5, 1,0)

# Confusion matrix
table(y_test,pred)

# get the accuracy score
prediction <- ifelse(pred != test$high_booking_rate, "wrong", "correct")
round(sum(prediction == "correct")/(sum(prediction =="correct")+sum(prediction =="wrong")), digits = 2)

#80% accuracy



####### tunning parameter:

# Pass in our hyperparameteres and train the model 
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     summaryFunction=twoClassSummary,	
                     classProbs=TRUE,
                     allowParallel = TRUE) 


grid <- expand.grid(interaction.depth=5,
                    n.trees=15,
                    shrinkage=0.15,
                    n.minobsinnode = 20)

GBM <- train(high_booking_rate ~., data=train, 
             method="gbm",
             trControl = ctrl,
             tuneGrid=grid,
             train.fraction = 0.5,
             tuneLength=3
)
# Create our prediction probabilities
preds<-predict(GBM,X_test)

#use cutoff=0.5 to classify it
pred<-ifelse(preds >=0.5, 1,0)
pred



library(ROCR)
# Create the confusion matrix
table(y_test,pred)
prediction <- ifelse(pred != test$high_booking_rate, "wrong", "correct")

# get the accuracy score
round(sum(prediction == "correct")/(sum(prediction =="correct")+sum(prediction =="wrong")), digits = 2)


####################################################################################################################################

#6.KNN
##### no select any features as first
# KNN needs to scale dataX
airbnb[,-1]=scale(airbnb[,-1])
# remove all columns with NaN
airbnb=airbnb[,colSums(is.na(airbnb))==0]

# Set the seed to create reproducible train and test sets
set.seed(1234)

# Create a stratified random sample to create train and test sets
# Reference the outcome variable
trainIndex   <- createDataPartition(airbnb$high_booking_rate, p=0.8, list=FALSE, times=1)

train        <- airbnb[ trainIndex, ]
test         <- airbnb[-trainIndex, ]


#specify X_train, y_train, X_test, y_test
X_train=train[,-1]
y_train=train[,1]

X_test=test[,-1]
y_test=test[,1]


## use "knn" we learned from class

library(class)

##try k=5 (default)
#for training data
knn.pred_train5=knn(X_train,X_train,y_train,k=5)
train_acc1=sum(ifelse(knn.pred_train1==y_train,1,0))/nrow(ab_train)
#for testing data
knn.pred_test5=knn(X_train,X_test,y_train,k=5)


#Overall error rate.
mean(knn.pred_test5 !=y_test)
# Confusion matrix
table(knn.pred1,c.valid)
# accuracy on test
test_acc5=sum(ifelse(knn.pred_test5==y_test,1,0))/nrow(test)
test_acc5
#73.2% correct


##try k=7
#for training data
knn.pred_train7=knn(X_train,X_train,y_train,k=7)
train_acc1=sum(ifelse(knn.pred_train1==y_train,1,0))/nrow(ab_train)
#for testing data
knn.pred_test7=knn(X_train,X_test,y_train,k=7)
test_acc7=sum(ifelse(knn.pred_test7==y_test,1,0))/nrow(test)
test_acc7
#75.78% correct


##try k=10
#for training data
knn.pred_train10=knn(X_train,X_train,y_train,k=10)
train_acc10=sum(ifelse(knn.pred_train1==y_train,1,0))/nrow(ab_train)
#for testing data
knn.pred_test10=knn(X_train,X_test,y_train,k=10)
# accuracy on test
test_acc10=sum(ifelse(knn.pred_test7==y_test,1,0))/nrow(test)
test_acc10
#76.38%


## other method: find the best K automatically  using caret pakage
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
# knnFit <- train(high_booking_rate ~ ., data =ab_train , method = "knn", trControl = ctrl,tuneLength = 5,preProcess = c("center", "scale"))
#knnFit
knnFit <- train(X_train,y_train, method = "knn",trControl = ctrl,tuneLength = 5)
plot(knn_fit)
test_pred <- predict(knn_Fit, newdata = X_test)
test_pred



####### select features later to try again !
#1.load data
library(caret)
airbnb<-read.csv('C:/Users/Xin Gu/Desktop/BA courses/758T/project/Airbnb/fifth/scaled_sf_KNN1.csv')

# Create a stratified random sample to create train and test sets
# Reference the outcome variable
trainIndex   <- createDataPartition(airbnb$y, p=0.8, list=FALSE, times=1)

train        <- airbnb[ trainIndex, ]
test         <- airbnb[-trainIndex, ]



#specify X_train, y_train, X_test, y_test
X_train=train[,-1]
y_train=train[,1]

X_test=test[,-1]
y_test=test[,1]

## use "knn" we learned from class

library(class)

##try k=7
#for testing data
knn.pred_test7=knn(X_train,X_test,y_train,k=7)
test_acc7=sum(ifelse(knn.pred_test7==y_test,1,0))/nrow(test)
test_acc7
#75.78% correct

##try k=9
#for testing data
knn.pred_test9=knn(X_train,X_test,y_train,k=9)
test_acc9=sum(ifelse(knn.pred_test7==y_test,1,0))/nrow(test)
test_acc9
## 78.72% correct =k7

##try k=9
knn.pred_test10=knn(X_train,X_test,y_train,k=10)
test_acc9=sum(ifelse(knn.pred_test7==y_test,1,0))/nrow(test)
test_acc9
