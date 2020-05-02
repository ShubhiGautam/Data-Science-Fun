# loading the data
q1 <- startups
q1
attach(q1)
colnames(q1)<-c("RD","admin","marketing","state","profit")

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

pairs(q1_new)
cor(q1_new)

#Splitting the data into train and test sets

num <- nrow(q1_new)
num
n1 <- 0.7*num

q1_train = q1_new[sample(1:num,n1),]
q1_test = q1_new[-sample(1:num,n1),]


#Linear Model lm(Y~X1+X2+X3)
#model1
model1 <- lm(profit~RD+admin+marketing+state_florida+state_california+state_ny,data=q1_train)
summary(model1) #R^2 <- 0.9602
plot(model1)

# as we can see in the summary, this model has certain coefficients which have a high p value. Therefore, we must check which input has a high collinearity problem
library(carData)
library(car) #companion to apply regression
vif(model1) #variance inflation factor
alias(model1)


# from the model, we remove state_ny since it has less influence on profit compared to the other states

#model2
model2 <- lm(profit~RD+admin+marketing+state_florida+state_california,data=q1_train)
summary(model2) #R^2 <- 0.9602
vif(model2) #variance inflation factor

#variables are still not stastically significant, so we remove state_florida since it has the highest p-value

#model3
model3<- lm(profit~RD+admin+marketing+state_california,data=q1_train)
summary(model3) #R^2 <- 0.9613
vif(model3)

#removing state_california variable as it has the highest pvalue and is greater than 0.05

#model4
model4<- lm(profit~RD+admin+marketing,data=q1_train)
summary(model4) #R^2 <- 0.9618
vif(model4) #variance inflation factor


#admin still has a high pvalue, so will remove it from the model

#model5
model5 <- lm(profit~RD+marketing,data=q1_train)
summary(model5) #R^2 <- 0.9621
vif(model5) #variance inflation factor

#marketing still has a high pvalue so I remove it from the model

#model6
model6 <- lm(profit~RD,data=q1_train)
summary(model6) #R^2 <- 0.96


#predicting the values using the above model (on train data set)

predict_data <- predict(model6,data=q1_train)
err = q1_train$profit - predict_data
rmse1 <- sqrt(mean(err**2))
rmse1 #7073.42

#predicting the values on test data set
predict_test <- predict(model6,q1_test)
err2= q1_test$profit - predict_test
rmse2 <- sqrt(mean(err2**2))
rmse2 #11625.52
