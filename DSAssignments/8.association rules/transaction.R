#importing the dataset
library(readr)
data<-read_csv("C://Users/Rajesh/Documents/me/data_assignments/mod15/transactions_retail1.csv")
head(data)
View(data)
attach(data)

retail<-as(data,"transactions")
class(retail)
summary(retail)

## EDA
dim(data)
str(data) #not all data is numeric
colnames(data)
is.na(data) #none
summary(data)
class(data)

#association rules
install.packages("arules")
library("arules") # Used for building association rules i.e. apriori algorithm

# install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules

# Building rules using apriori algorithm
arules<-apriori(retail,parameter = list(support=0.002,confidence=0.5,minlen=3))
arules
#writing ... [1110 rule(s)] done [0.00s].

arules<-apriori(retail,parameter = list(support=0.01,confidence=0.5,minlen=2))
arules
#writing ... [16 rule(s)] done [0.00s].

arules<-apriori(retail,parameter = list(support=0.008,confidence=0.6,minlen=4))
arules
#writing ... [0 rule(s)] done [0.00s].





#sorting by lift ratio
inspect((sort(arules,by="lift")))
#looking at first 6 rules
inspect(head(sort(arules,by="lift")))

# Overal quality -> looks at all the parameters
head(quality(arules))

plot(arules)
plot(arules,jitter=0)

# Different Ways of Visualizing Rules
plot(arules)
windows()
plot(arules,method="grouped")
plot(arules[1:16],method = "graph") # for good visualization try plotting only few rules


write(arules, file="transaction_arules.csv",sep=",")

getwd()