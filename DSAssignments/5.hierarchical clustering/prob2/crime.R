#importing the dataset
library(readr)
data<-read_csv("C://Users/Rajesh/Documents/me/data_assignments/mod12/crime_data.csv")
head(data)
View(data)
attach(data)


## EDA
dim(data)
str(data) #all data is numeric except for first col (X1)
colnames(data)
is.na(data) #none
summary(data)
plot(Murder,Assault)
plot(Murder,Rape )
plot(Rape,UrbanPop)

#standardizing the data
normalized_data <- scale(data[,2:5])
View(normalized_data)

#distance matrix
d <-dist(normalized_data,method="euclidean")

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

write.csv(data,file="C://Users/Rajesh/Documents/me/data_assignments/mod12/cluster_crime.csv")

