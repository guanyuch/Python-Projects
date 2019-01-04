## This Part aims to select features: 
##Calculate Correlation; Lasso


## 1. Remove Redundant Features
# load the data
data(Train_XY_AllNumeric_4.26.csv)

# ensure the results are repeatable
set.seed(11217)
# load the library
install.packages("mblench")
install.packages("caret")
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix <- cor(Train_XY_AllNumeric_4_26[,2:22])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)



## 2. LASSO
## select variables using lasso
Train_XY_AllNumeric_4_26$high_booking_rate=as.numeric(Train_XY_AllNumeric_4_26$high_booking_rate)

set.seed(11217)

## select variables(Without Spliting Data)
install.packages("glmnet",dependencies=TRUE)
library(glmnet)
set.seed(11217)
glmnet_lasso = glmnet(as.matrix(Train_XY_AllNumeric_4_26[c(2:311)]),Train_XY_AllNumeric_4_26$high_booking_rate,alpha=1,family = "binomial")
glmnet_lasso.cv=cv.glmnet(as.matrix(Train_XY_AllNumeric_4_26[c(2:311)]),Train_XY_AllNumeric_4_26$high_booking_rate,alpha=1)
plot(glmnet_lasso.cv)
best.lambda=glmnet_lasso.cv$lambda.min

lasso.pred = predict(glmnet_lasso,s=best.lambda,newx=as.matrix(Train_XY_AllNumeric_4_26[c(2:311)]))
sqrt(mean((Train_XY_AllNumeric_4_26$high_booking_rate-lasso.pred)^2))
c <- predict(glmnet_lasso,s=best.lambda,type="coefficients")
inds <- which(c!=0)
variables <- row.names(c)[inds]

