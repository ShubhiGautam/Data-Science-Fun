#predicting the type of program a student is in based on other attributes

data <- read.csv('C:/Users/Rajesh/Downloads/data_assignments/mod10/mdata.csv')
View(data)
data <- data[,-1]
data <- data[,-1]
View(data)
attach(data)

## EDA
dim(data)
str(data)
unique(female)
unique(prog) #this is what we need to predict
unique(ses)
colnames(data)
is.na(data)
head(data)


summary(data)
head(data)

plot(data$read)
plot(data$write)
plot(data$math)
plot(data$female,math)
plot(data$female,write)
plot(data$female,read)
plot(data$prog)
plot(data$honors)
plot(data$schtyp)
plot(data$ses,data$prog)

#since we're looking at the kind of program a student is in (multiple categories) based on other attributes which are not ordered, we need to implement multinomial logit model
#multinomial logit model
#packages required
install.packages('mlogit')
require('mlogit')
require('nnet')

table(data$prog) #tabular representation of y categories
View(data)

data.prog <- multinom(prog ~ honors + female + ses + schtyp + read + write + math + science, data=data)
summary(data.prog)

#changing the baseline to general & reexecute model
data$prog <- relevel(data$prog, ref="general")

# are my coefficients significant or not?
# how to find that p value?
# first find the z values 
z <- summary(data.prog)$coefficients / summary(data.prog)$standard.errors
#my z values are both positive and negative but with pnorm we get only those to left of the bell curve
# so to get the right side we do 1-pnorm
# that gives me alpha/2, but we need alpha so we multiply that by 2 hence getting the eqn below
p_value <- (1-pnorm(abs(z),0,1))*2

summary(data.prog)$coefficients
p_value
#we see a lot of insignificant coefficients (>0.05)

# to get true coefficients, apply anti log
exp(coef(data.prog))

# predict probabilities
prob <- fitted(data.prog)
prob

# Find the accuracy of the model
class(prob)
prob <- data.frame(prob)
View(prob)
prob["pred"] <- NULL

# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}

pred_name <- apply(prob,1,get_names)
?apply
prob$pred <- pred_name
View(prob)

# Confusion matrix
table(pred_name,data$prog)

# confusion matrix visualization
barplot(table(pred_name,data$prog),beside = T,col=c("red","lightgreen","blue","orange"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")


# Accuracy 
mean(pred_name==data$prog) #62.5 %
