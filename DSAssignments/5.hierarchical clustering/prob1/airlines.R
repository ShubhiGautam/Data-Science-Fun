#importing the dataset
library(readxl)
data<-read_excel("C://Users/Rajesh/Documents/me/data_assignments/mod12/EastWestAirlines.xlsx",sheet="data")
head(data)
attach(data)

## EDA
dim(data)
str(data) #all data is numeric
colnames(data)
is.na(data) #none
#don't need the id variable therefore removing it
View(data)
data <- data[,-1]
View(data)
summary(data)
plot(data$Balance,data$Bonus_miles)
plot(data$Balance)
plot(data$Balance,data$Bonus_trans)
plot(data$Balance,data$Flight_miles_12mo)

#normalizing the data
head(data)
normalize<-function(x)
{
  return ((x-min(x))/max(x)-min(x))
}

#lapply is function applied for operations on list objects & returns a list object of same length of original set
data<-as.data.frame(lapply(data,normalize))
View(data)
summary(data)

#distance matrix
d <-dist(data,method="euclidean")

#creating the hierarchy
#using complete linkage function
fit<-hclust(d,method="complete")
#to look at the cluster dendogram
plot(fit)
#hang allows us to see the arrangement at the same level
plot(fit,hang=-1)  

#cutting the tree into 4 clusters
group <- cutree(fit,k=4)
unique(group)
rect.hclust(fit,k=4,border="red")

group 
View(group)
group<-as.matrix(group) #turn my row into a column
View(group)

final<-data.frame(group,data)
View(final)
unique(group)

getwd()

write.csv(data,file="cluster_airlines.csv")
