library(randomForest)
library(caret)  
library(ggplot2) 

airbnb<-read.csv('/Users/cql/Documents/2017美国/materials/BUDT 758T/Train_XY_AllNumeric_4.26.csv')



set.seed(1234)

## First, partition 20% of the data for testing datax, the remaining 80% for training model
train_instn = sample(nrow(airbnb), 0.8*nrow(airbnb))

ab_train <- airbnb[train_instn,]
ab_test <- airbnb[-train_instn,]

#Fit Random Forest Model
rf = randomForest(high_booking_rate ~ ., 
                  importance = TRUE,
                   ntree = 150,
                   data = ab_train, mtry=16)
plot(rf)
print(rf)

importance(rf,type=2)
# Predicting response variable
ab_test$predicted.response <- predict(rf ,ab_test)
# Create Confusion Matrix
print(
  confusionMatrix(data=ab_test$predicted.response,
                  reference=ab_test$high_booking_rate))

#fit logistic model
##this is how you train a logistic regression model
fit2 <- glm(ab_train$high_booking_rate~.,data=ab_train,family="binomial")
summary(fit2)

install.packages('xtable')

idx <- order(coef(summary(fit2))[,4])  # sort out the p-values
idx

##note the slight change to the predict function here
log_preds <- predict(fit2,newdata=ab_test,type="response")

##do the logistic classification
log_class <- ifelse(log_preds>.5,1,0)

##make a confusion matrix
table(ab_test$high_booking_rate,log_class)
