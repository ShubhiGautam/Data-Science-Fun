#importing the dataset
library(readr)
data<-read_csv("C://Users/Rajesh/Documents/me/data_assignments/mod15/my_movies.csv")
head(data)
View(data)
attach(data)


## EDA
dim(data)
str(data) #not all data numeric
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
arules <- apriori(as.matrix(data[,6:15]), parameter = list(support=0.02,confidence=0.6,minlen=2))
arules
#writing ... [81 rule(s)]

arules <- apriori(as.matrix(data[,6:15]), parameter = list(support=0.001,confidence=0.6,minlen=3))
arules
#writing ... [66 rule(s)]

arules <- apriori(as.matrix(data[,6:15]), parameter = list(support=0.004,confidence=0.6,minlen=4))
arules
#writing ... [29 rule(s)]

arules<-apriori(as.matrix(data[,6:15]),parameter = list(support=0.08,confidence=0.7,minlen=3))
arules
#writing ... [65 rule(s)]

#creating item sets
gi2<-generatingItemsets(arules)
gi2

#which rules are duplicated
d11<-which(duplicated(gi2))
d11
arules<-arules[-d11]
arules #now 7 rules  

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
plot(arules[1:7],method = "graph") # for good visualization try plotting only few rules


write(arules, file="a_rules.csv",sep=",")

getwd()