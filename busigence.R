
#importing training data

data1 <- read.csv('C:/Users/akmeh/Documents/R/customersorder_train.csv',na.strings=c("","NA"))

#importing test data

data10 <- read.csv('C:/Users/akmeh/Documents/R/customersorder_test.csv',na.strings=c("","NA"))

# Exploring the target variable

table(data1$F36)

# Filtering training data to get continuous variables in the target variable

attach(data1)
data2 <- data1[which(data1$F36 != 'long' & data1$F36 != 'N'),]
detach(data1)
nrow(data1)
nrow(data2)

# Removing the identifier variable

data3 <- data2[,-c(23)]

# Checking for any missing values

data4 <- data3[complete.cases(data3),]
nrow(data3)
nrow(data4)

# Checking the class of all predictor/response variables to prep before applying linear regression

lapply(data4,class)
split(names(data4),sapply(data4, function(x) paste(class(x), collapse=" ")))

# Target varaible is convrted to numeric value so that we can apply linear regression

data4$F36 <- as.numeric(data4$F36)

# Excluding the other non-numeric variable

data5<- data4[,-c(412)]

# Checking the class again

split(names(data5),sapply(data5, function(x) paste(class(x), collapse=" ")))

# Getting very poor value of R-Square of .08 after applying linear regression

lm.fit <- lm(F36 ~ .,data=data5)
summary(lm.fit)

# Attempted Stepwise Regression to extract important features but R got stuck due to presence of too many variables

# library(MASS)
# fit <- lm(F36~.,data=data5)
# step <- stepAIC(fit, direction="both")
# step$anova # display results

# Now using Lasso regression as it is apt alogorithm to deal with data having too many predictor variables

library(glmnet)
predictor_variables <- data5[,-c(35)]
predictor_variables <- as.matrix(predictor_variables)
no_days <- data5$F36

lasso<-cv.glmnet(predictor_variables,no_days,family="gaussian",alpha=1)

#min value of lambda

lambda_min <- lasso$lambda.min

#best value of lambda

lambda_1se <- lasso$lambda.1se
print(lasso)

# Drawing some plots to visualize better

plot(lasso,xvar="lambda",label=T)
plot(lasso)
lasso$lambda[which.min(lasso$cvm)]
coef(lasso)

# Now loading test data 

#data10$F36 <- as.numeric(data10$F36)

# Similarly filtering for continuous variables

data11 <- data10[which(data10$F36 != 'long' & data10$F36 != 'N'),]
data11$F36 <- as.numeric(data11$F36)
data12 <- data11[,-c(23,413,36)]
test_matrix <- as.matrix(data12)
no_days <- data11$F36

# Predicting using the model built on training data

lasso2 <-predict(lasso,newx = test_matrix,type = 'response',s=lambda_1se)
lasso.resid<-lasso2-data11$F36

# Calculating mean squared error

mean(lasso.resid^2)

# Now using decision trees to predict values

# Test data

data13 <- data11[,-c(23,413)]

# Delecting the columns missing in the test data

# Training data

data6 <- data5[,-which(names(data5) %in% c("F105","F106","F107","F108","F109","F110"))]

# Building the decision tree

library(rpart)
fit <- rpart(F36 ~ .,
             method="anova", data=data6)

summary(fit)

# Pruning the decision tree

pr.dt <- predict(fit,data13)
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
pr.dt <- predict(pfit,data13)

# Calculating mean squared error

MSE.dt <- sum((pr.dt - data13$F36)^2)/nrow(data13)

# We see that mean squared error is close to 5208 same as that for lasso regression model built earlier

RMSE <- sqrt(MSE.dt)

# Now trying to predict for categorical variables

data14 <- data1[which(data1$F36 != 'long' & data1$F36 != 'N'),]

# For number if days less than 180, assign S, this is because we need to run logistic regression model and hence we need all categorical variables as input

# Preparing training data

data14$F36 <- 'S'
data15 <- data1[which(data1$F36 == 'long' | data1$F36 == 'N'),]
data16 <- rbind(data14,data15)
nrow(data16)

predictor_variables <- data16[,-c(23,413,36)]
predictor_variables <- as.matrix(predictor_variables)

table(data16$F36)

# Using multinomial lasso regression as we have more than 2 levels in target variable

lasso<-glmnet(predictor_variables,data16$F36,family='multinomial', alpha=1)

# #min value of lambda
# lambda_min <- lasso$lambda.min
# #best value of lambda
# lambda_1se <- lasso$lambda.1se

# Similarly preparing test data

data17 <- data10[which(data10$F36 == 'long' | data10$F36 == 'N'),]
data11$F36 <- 'S'
data18 <- rbind(data11,data17)
nrow(data18)
data19 <- data18[,-c(23,413,36)]
test_matrix <- as.matrix(data19)
no_days <- data18$F36
lasso2 <-predict(lasso,newx = test_matrix,type = 'response',s=1.292e-01)


lasso3 <- data.frame(lasso2)
names(lasso3)[1] <- "long"
names(lasso3)[2] <- "N"
names(lasso3)[3] <- "S"

lasso3$max <- apply(lasso3, 1, FUN=max)

# Assigning output class value to the class with the highest probability

lasso3$col_name <- colnames(lasso3)[apply(lasso3,1,which.max)]

# Calculating misclassificationerror

misClasificError <- mean(lasso3$col_name != data18$F36,na.rm = TRUE)

# We are getting an accuracy of 0.30847 

print(paste('Accuracy',1-misClasificError))

# Loading the predictions csv

predictions <- read.csv('C:/Users/akmeh/Documents/R/predictions.csv',na.strings=c("","NA"))

for (i in 1:2347)
{
  predictions[i,3] <- ifelse(lasso3[i,5]!="S",lasso3[i,5],lasso2[i])
}