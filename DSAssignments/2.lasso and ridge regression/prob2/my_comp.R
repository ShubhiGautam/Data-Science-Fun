# loading the data
computer <- comp
computer
attach(computer)

#removing first col
computer <- computer[-1]
computer

#converting non numberic data into numeric#
computer$cd=ifelse(computer$cd=='yes',1,0)
computer$multi=ifelse(computer$multi=='yes',1,0)
computer$premium=ifelse(computer$premium=='yes',1,0)

attach(computer)
computer


# EDA

dim(computer) #dimension of the data
str(computer) #structure of the data, chr struct of state
is.na(computer) # is there any data that is missing? 
str(computer)

summary(computer)

plot(computer$speed,computer$price) 
plot(computer$hd,computer$price) 
plot(computer$ram,computer$price) 
plot(computer$screen,computer$price) 
plot(computer$ads,computer$price) 
plot(computer$trend,computer$price) 

#understanding the correlationso f the data
plot(computer)
cor(computer)

#converting the data into compatible format in which model accepts 
#model matrix of the data
computer_x <- model.matrix(price~.-1,data=computer)
computer_y <- computer$price

# glmnet automatically selects the range of ?? values
# setting lamda as 10^10 till 10^-2
lambda <- 10^seq(10, -2, length = 50)

#Splitting the data into train and test sets

num <- nrow(computer)
num
n1 <- 0.7*num

computer_train = computer[sample(1:num,n1),]
computer_test = computer[-sample(1:num,n1),]

x_train = model.matrix(price~.-1,data=computer_train)
y_train = computer_train$price

x_test = model.matrix(price~.-1,data=computer_test)
y_test = computer_test$price
  
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
best_lambda #0.9435724


#predicting on test data using lambda
computer_pred <- predict(lasso_reg,s=best_lambda,newx=x_test)
computer_pred

mean(computer_pred-y_test)^2 #[1] [1] 49.35847


##RIDGE REGRESSION

#fitting ridge model onto training data
ridge_reg = glmnet(x_train,y_train,alpha=0, Lambda=lambda)
summary(ridge_reg)
#drawing plots of coefficients to see how coeff vary with lambda
plot(ridge_reg,xvar="lambda",Label=T)


#performinng cross validation (cv) aka data is split into n folds/groups and run analysis on each fold & then avg the overall error estimate
#cv is a procedure used to evaluate models on a limited data sample
cv.out = cv.glmnet(x_train,y_train,alpha=0,Lambda=lamba,nfolds=5)

plot(cv.out) #cv.glmnet does k-fold cross validation for glmnet, produces a plot, and returns a value for lamba
cv.out

#selecting the best value of lambda for which we're gettin best model
best_lambda_rid = cv.out$lambda.min
best_lambda_rid #3722.92


#predicting on test data using lambda
ridge_pred <- predict(ridge_reg,s=best_lambda_rid,newx=x_test)
ridge_pred

mean(ridge_pred-y_test)^2 #[1] [1] 75.4046


#using lasso because less error
final=glmnet(computer_x,computer_y,alpha=1,lambda=lambda)

#display coefficients using lambda choosen by cv#
lasso_coeff=predict(final,type="coefficients",s=best_lambda)[1:10,]
lasso_coeff



