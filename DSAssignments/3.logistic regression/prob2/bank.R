data = read.csv("C:/Users/Rajesh/Downloads/data_assignments/mod9/bank_data.csv")
head(data)
View(data)
attach(data)

## EDA
dim(data)
str(data)
is.na(data)

summary(data)
head(data)
colnames(data)

plot(data$y)
plot(data$age)
plot(data$duration)
plot(data$balance)
plot(data)


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1

#preparing my model
colnames(data)
model <- glm(y~.,data=data, family='binomial')
summary(model)
#null deviance is 32631 (Y=B0 aka average)
#residual devaince is 22640 (Y=B0+ other variabels (b1x1,b2x2,etc))
#residual dev is less than null as expected, and the difference is large thus the variables are quality variables
# AIC value is 22696, model with least AIC value is best

#the intercept values we get is log of intercepts (because of Y and X relationship)
#to get the original B values we apply the formula before
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))


#prediction, use 'response' to get probability of y
prob <- predict(model,data,type="response")
prob

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,data$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 0.9005552


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
pred_values <- ifelse(prob>0.5,1,0)



# Creating new column to store the above values
data[,"prob"] <- prob
View(data)
data[,"pred_values"] <- pred_values


View(data[,c(1,9:11)])

table(data$y,data$pred_values)

library(ROCR)

rocrpred<-prediction(prob,data$y)
rocrperf<-performance(rocrpred,'tpr','fpr') #false positive rate, true positive rate
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
#fpr should be least , and highest tpr

install.packages('dplyr')
library(dplyr)
#arrange it so that only 6 vals showing and descending order of TPR
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

