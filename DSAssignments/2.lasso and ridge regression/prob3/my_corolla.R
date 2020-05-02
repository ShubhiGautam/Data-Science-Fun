#Predicing the price according to specific variables

#loading the data
corolla <- corolla
attach(corolla)
View(corolla)
#EDA

dim(corolla) #dimension of the data
str(corolla) #structure of the data, chr struct of state
is.na(corolla) # is there any data that is missing? 

summary(corolla)

# only looking at the variables that we need
plot(corolla$Age_08_04,corolla$Price) # we notice a decreasing trend as age increases, negative correlation in price
plot(corolla$KM,corolla$Price)# majority of the data is at lower price and lower KM
plot(corolla$HP,corolla$Price)
plot(corolla$cc,corolla$Price)
plot(corolla$Doors,corolla$Price)
plot(corolla$Gears,corolla$Price)
plot(corolla$Quarterly_Tax,corolla$Price)
plot(corolla$Weight,corolla$Price) #majority of data at lower weight and lower price

head(corolla)
str(corolla)
unique(Fuel_Type)
unique(Color)
unique(Model)


#creating dummy variables for the categorical data
corolla$Fuel_Type<-as.factor(corolla$Fuel_Type)
str(corolla)
View(corolla)
corolla$Fuel_Type <- unclass(corolla$Fuel_Type)
View(corolla)

corolla$Color<-as.factor(corolla$Color)
str(corolla)
View(corolla)
corolla$Color <- unclass(corolla$Color)
View(corolla)

corolla$Model<-as.factor(corolla$Model)
str(corolla)
View(corolla)
corolla$Model <- unclass(corolla$Model)
View(corolla)
  
#model matrix for total data set#
corolla_x<-model.matrix(Price~.-1,data=corolla)
corolla_y<-corolla$Price

#Splitting the data into train and test sets

num <- nrow(corolla)
num
n1 <- 0.7*num

#model matrix

cor_train = corolla[sample(1:num,n1),]
cor_test = corolla[-sample(1:num,n1),]

x_train <- model.matrix(Price~.-1,data=cor_train)
y_train <- cor_train$Price

x_test <-model.matrix(Price~.-1,data=cor_test)
y_test <-cor_test$Price

#lambda#
lambda<-10^seq(10,-2,length=50)

###LASSO REGRESSION###
#fit lasso model on training data#
lasso_mod=glmnet(x_train,y_train,alpha = 1,lambda = lambda)
plot(lasso_mod)
cv.out=cv.glmnet(x_train,y_train,alpha=1)
plot(cv.out)

bestlam_lasso=cv.out$lambda.min
bestlam_lasso #1] 80.80102

#predicting on test data using best lambda#
lasso_pred=predict(lasso_mod,s=bestlam_lasso,newx=x_test)
mean((lasso_pred-y_test)^2) # 1253172


###RIDGE REGRESSION#
ridge_mod=glmnet(x_train,y_train,alpha=0,lambda=lambda)
plot(ridge_mod)

cv.out=cv.glmnet(x_train,y_train,alpha=0)
plot(cv.out)

bestlam_ridge=cv.out$lambda.min
bestlam_ridge

ridge_predict<-predict(ridge_mod,s=bestlam_ridge,newx=x_test)
mean((ridge_predict-y_test)^2) #1251686

#since ridge has a lower error we use ridge on the entire data set
final=glmnet(corolla_x,corolla_y,alpha=0,lambda=lambda)

#display coefficients using lambda choosen by cv#
lasso_coeff=predict(final,type="coefficients",s=bestlam_ridge)[1:48,]
lasso_coeff


