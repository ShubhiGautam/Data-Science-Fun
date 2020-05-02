#Predicing the sales of the computer

#loading the data
comp <- comp
attach(comp)

#EDA

dim(comp) #dimension of the data
str(comp) #structure of the data, chr struct of state
is.na(comp) # is there any data that is missing? 

summary(comp)

plot(comp$speed,comp$price) 
plot(comp$hd,comp$price) 
plot(comp$ram,comp$price) 
plot(comp$screen,comp$price) 
plot(comp$ads,comp$price) 
plot(comp$trend,comp$price) 



#removing first col of data as it's not needed

comp <- comp[-1]
comp

#creating dummy variables for the categorical data
comp$cd = ifelse(comp$cd == 'yes',1,0)
comp$multi = ifelse(comp$cd == 'yes',1,0)
comp$premium = ifelse(comp$cd == 'yes',1,0)

plot(comp$cd,comp$price) 
plot(comp$multi,comp$price) 
plot(comp$premium,comp$price) 

comp
attach(comp)

#understanding correlations of data
pairs(comp)
cor(comp)

#splitting the data into train , test data sets
num = nrow(comp)
n1 = 0.7*num
comp_train = comp[sample(1:num,n1),]
comp_test = comp[-sample(1:num,n1),]

#build model using training data set
model1 <- lm('price~speed+hd+ram+screen+cd+multi+premium+ads+trend',data=comp_train)
summary(model1)  
#R2 <- 0.7172
# we can also see that the p value of all variables are below 0.05 hence they are significant
vif(model1)
alias(model1)  
#no strong dependency between variables as all are <10  

#removing multi and premium from the as they don't have much influence on the price according to their p values
model2 <- lm('price~speed+hd+ram+screen+cd+ads+trend',data=comp_train)
summary(model2) #R^2 <- 0.7172
vif(model2) #again model2 has a all variables <10 so no collinearity and all p values are less than 0.05 so we use the rest of the variables

#predict the price using train data
predict_train=predict(model2,data=comp_train)
err_train=comp_train$price-predict_train
rmse_train=sqrt(mean(err_train*err_train))
rmse_train #307.8246

#predict the price using test data set
predict_test=predict(model2,comp_test)
err_test=comp_test$price-predict_test
rmse_test=sqrt(mean(err_test*err_test))
rmse_test #306.5664

