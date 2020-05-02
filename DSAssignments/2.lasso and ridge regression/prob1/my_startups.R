# loading the data
q1 <- startups
q1
attach(q1)
colnames(q1)<-c("RD","admin","marketing","state","profit")
attach(q1)

# EDA

dim(q1) #dimension of the data
str(q1) #structure of the data, chr struct of state
is.na(q1) # is there any data that is missing? 

summary(q1)
plot(q1$RD,q1$profit) #profit seems to increase as RD increases
plot(q1$admin,q1$profit) # data is dispersed
plot(q1$marketing,q1$profit) # data observed at extreme ends
plot(q1$state,q1$profit) 


#creating dummy variabes
library(dummies)
q1_new <- dummy.data.frame(q1,sep="_")
colnames(q1_new) <- c("RD", "admin", "marketing", "state_california", "state_florida", "state_ny", "profit")
attach(q1_new)
q1_new
str(q1_new)

plot(q1_new)
cor(q1_new)

#converting the data into compatible format in which model accepts 
#model matrix of the total data set
startups_x = model.matrix(profit~.-1,data=q1_new)
startups_y = q1_new$profit
library(glmnet)

# glmnet automatically selects the range of ?? values
# setting lamda as 10^10 till 10^-2
lambda <- 10^seq(10, -2, length = 50)

#Splitting the data into train and test sets

num <- nrow(q1_new)
num
n1 <- 0.7*num

startups_train<-q1_new[sample(1:num,n1),]
startups_train
startups_test<-q1_new[-sample(1:num,n1),]

x_train<-model.matrix(profit~.-1,data=startups_train)
x_train
y_train <- startups_train$profit

x_test<-model.matrix(profit~.-1,data=startups_test)
y_test<-startups_test$profit


##REGULARIZATIONS

#Lasso Regression
#fitting lasso model onto training data
lasso_reg = glmnet(x_train,y_train,alpha=1, Lambda=lambda)
summary(lasso_reg)
#drawing plots of coefficients to see how coeff vary with lambda
plot(lasso_reg,xvar="lambda",Label=T)


#performinng cross validation (cv) aka data is split into n folds/groups and run analysis on each fold & then avg the overall error estimate
#cv is a procedure used to evaluate models on a limited data sample
cv.out = cv.glmnet(x_train,y_train,alpha=1,Lambda=lamba,nfolds=10)

plot(cv.out) #cv.glmnet does k-fold cross validation for glmnet, produces a plot, and returns a value for lamba
cv.out

#selecting the best value of lambda for which we're gettin best model
best_lambda = cv.out$lambda.min
best_lambda #1728.026


#predicting on test data using lambda
startups_pred <- predict(lasso_reg,s=best_lambda,newx=x_test)
startups_pred

mean(startups_pred-y_test)^2 #[1] 19368450

##RIDGE REGRESSION

#fitting ridge model onto training data
ridge_reg = glmnet(x_train,y_train,alpha=0, Lambda=lambda)
summary(ridge_reg)
#drawing plots of coefficients to see how coeff vary with lambda
plot(ridge_reg,xvar="lambda",Label=T)


#performinng cross validation (cv) aka data is split into n folds/groups and run analysis on each fold & then avg the overall error estimate
#cv is a procedure used to evaluate models on a limited data sample
cv.out = cv.glmnet(x_train,y_train,alpha=0,Lambda=lamba,nfolds=10)

plot(cv.out) #cv.glmnet does k-fold cross validation for glmnet, produces a plot, and returns a value for lamba
cv.out

#selecting the best value of lambda for which we're gettin best model
best_lambda_rid = cv.out$lambda.min
best_lambda_rid #4087.17


#predicting on test data using lambda
ridge_pred <- predict(ridge_reg,s=best_lambda_rid,newx=x_test)
ridge_pred

mean(ridge_pred-y_test)^2 #[1] 21045121



#since Lasso has lower R2 value, we fit lasso model onto full dataset
final = glmnet(startups_x,startups_y,alpha=1,Lambda=lambda)

#display coefficients using lambda choosen by cv#
lasso_coeff=predict(final,type="coefficients",s=best_lambda)[1:7,]
lasso_coeff

