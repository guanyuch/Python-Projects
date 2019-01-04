##-------------------------------------------------------------
## 1. Classification Tree

## Import Data
library(readr)
airbnb <- read_csv("C:/Users/janes/Google Drive/BUDT758T/project/final/Train_XY_AllNumeric_4.26.csv")
airbnb <-na.omit(airbnb)

library(rpart)                     # R package for decision Tree
library(caret)                     # R package for decision Tree

# Convert to Factor
col_names <- names(airbnb)
airbnb[,col_names] <- lapply(airbnb[,col_names] , factor)

set.seed(12345)

## First, partition 20% of the data for testing data
test_instn = sample(nrow(airbnb), 0.2*nrow(airbnb))
airbnb_test <- airbnb[test_instn,]

## Save the rest of the data as the data that isn't testing
airbnb_train <- airbnb[-test_instn,]

library(caret)
t.cont<-rpart.control(minsplit=2,cp=0.5,xval=5)
mtree  <- rpart(high_booking_rate~ .,data=airbnb_train,method = "class",control=t.cont)
best.cp<-mtree$cptable[which.min(mtree$cptable[,'xerror']),'CP']
pruned_mtree<-prune.rpart(mtree,cp=best.cp)

#Confusion matrix
rpartpred <- predict(pruned_mtree,airbnb_test,type="class")
confusionMatrix(rpartpred,airbnb_test$high_booking_rate)

#Plot tree
plot(mtree)
#Lable on Decision Tree
text(mtree)

printcp(pruned_mtree)

##-----------------------------------------------------------
## 2. XGBoost 

# load data
airbnb<-read.csv('C:/Users/janes/Google Drive/BUDT758T/project/final/Train_XY_AllNumeric_4.26.csv')
nrow(airbnb)
ncol(airbnb)

# Load the caret package
library(caret)

# Set the seed to create reproducible train and test sets
set.seed(1234)

# Create a stratified random sample to create train and test sets
# Reference the outcome variable
trainIndex   <- createDataPartition(airbnb$high_booking_rate, p=0.8, list=FALSE, times=1)
train        <- airbnb[ trainIndex, ]
test         <- airbnb[-trainIndex, ]

# Create separate vectors of our outcome variable for both our train and test sets
# We'll use these to train and test our model later
train.label  <- train$high_booking_rate
test.label   <- test$high_booking_rate

# Load the Matrix package
library(Matrix)

# Create sparse matrixes and perform One-Hot Encoding to create dummy variables
dtrain  <- sparse.model.matrix(high_booking_rate ~ .-1, data=train)
dtest   <- sparse.model.matrix(high_booking_rate ~ .-1, data=test)

# View the number of rows and features of each set
dim(dtrain)
dim(dtest)

# Load the XGBoost package
library(xgboost)

# Set our hyperparameters
param <- list(objective   = "binary:logistic",
              max_depth   = 4,
              eta         = 1,
              gammma      = 0,
              colsample_bytree = 1,
              min_child_weight = 6)

set.seed(1234)

# Pass in our hyperparameteres and train the model 
system.time(xgb <- xgboost(params  = param,
                           data    = dtrain,
                           label   = train.label, 
                           nrounds = 3000,
                           print_every_n = 100,
                           verbose = 1))

# Create our prediction probabilities
pred <- predict(xgb, dtest)

# Set our cutoff threshold
pred <- ifelse(pred >= 0.5, 1, 0)

install.packages("ROCR")
library(ROCR)

# Create the confusion matrix
prediction <- ifelse(pred != test.label, "wrong", "correct")

round(sum(prediction == "correct")/(sum(prediction =="correct")+sum(prediction =="wrong")), digits = 2)



