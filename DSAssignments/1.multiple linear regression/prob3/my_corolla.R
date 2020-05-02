#Predicing the price according to specific variables

#loading the data
corolla <- corolla
attach(corolla)

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

#creating new dataset we need
corolla_new=corolla[c("Price", "Age_08_04", "KM", "HP", "Quarterly_Tax", "Weight", "Automatic_airco","Guarantee_Period" , "Powered_Windows")]
head(corolla_new)


#Splitting the data into train and test sets

num <- nrow(corolla_new)
num
n1 <- 0.7*num

cor_train = corolla_new[sample(1:num,n1),]
cor_test = corolla_new[-sample(1:num,n1),]

#building the model
model1= lm('Price~Age_08_04+KM+HP+Quarterly_Tax+Weight+Automatic_airco+Guarantee_Period+Powered_Windows',data= cor_train)
summary(model1) #R2 is 0.8935

#as we can see that all the variables have a p value which are less than 0.05 thus they are all significant

#checking for collinearity
library(carData)
library(car) #companion to apply regression
vif(model1) #variance inflation factor

# all the values are less than 10. Therefore, this means the variables aren't collinear either so we can keep all the variables.

#predict the price on train data set
predict_train=predict(model1,data=cor_train)
err_train=cor_train$Price-predict_train
rmse_train=sqrt(mean(err_train*err_train))
rmse_train #1204.527

#predict the price on test data set
predict_test=predict(model1,data=cor_test)
err_test=cor_test$Price-predict_test
rmse_test=sqrt(mean(err_test*err_test))
rmse_test #5455.183





