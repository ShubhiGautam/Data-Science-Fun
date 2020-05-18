#importing dataset
library(readr)
data <- read_csv("C:/Users/Rajesh/Documents/me/data_assignments/mod14/wine.csv")
View(data)
attach(data)

data1<-data[,-1] #remove first col cause we don't need
View(data1)
attach(data1)

## EDA
dim(data1)
str(data1)
colnames(data1)
is.na(data1) 
sum(is.na(data1))
summary(data1)

#using correlation matrix
#scores on each principle component should be calculated
pcaObj<-princomp(data1, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
summary(pcaObj)

#blank value indicates that when it's very close to 0 it's not considered.

#to see the weights use loadings func
loadings(pcaObj)

#graph to show importance of principal components
plot(pcaObj) #as expcted comp1 > other comps

biplot(pcaObj) #projects similarities in the variance

#square subdev to get variance 
#denom -> the total variance in our pcaObj
#cumsum -> the cumulative of say 1st,2nd then 1,2,3 etc
#essentially measures proportions of the variance with respect to each pc
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")

pcaObj$scores
# i will take 6 components as it captures ~88% of data
pcaObj$scores[,1:6]

# Top 6 pca scores 
#binding it to "type"
final<-cbind(data[,1],pcaObj$scores[,1:6])
View(final)

#this is the dataset that I will use for clustering.

#using it on final dataset - which has been dimensionally reduced
#k-means clustering
#using elbow curve just to decide the kvalue
wss = (nrow(final)-1)*sum(apply(final, 2, var))		 # Determine number of clusters by scree-plot 
wss #495
for (i in 1:8) wss[i] = sum(kmeans(final, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clust")
#the k cluster I am getting is 3 clusters

#non-hierarchical clustering
#distance matrix
d <-dist(final,method="euclidean")
#creating the hierarchy
#using complete linkage function
fit<-hclust(d,method="complete")
#to look at the cluster dendogram
plot(fit)
#hang allows us to see the arrangement at the same level
plot(fit,hang=-1)  
#cluster dendogram also showing that 3 clusters would be appropriate


#using it on data1 dataset - which hasnot been dimensionally reduced

#k-means clustering
#using elbow curve just to decide the kvalue
wss = (nrow(data1)-1)*sum(apply(data1, 2, var))		 # Determine number of clusters by scree-plot 
wss #495
for (i in 1:8) wss[i] = sum(kmeans(data1, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clust")
#the k cluster I am getting is 3 clusters - exactly the same as dimensionally reduced data


#non-hierarchical clustering
#distance matrix
d <-dist(data1,method="euclidean")
#creating the hierarchy
#using complete linkage function
fit<-hclust(d,method="complete")
#to look at the cluster dendogram
plot(fit)
#hang allows us to see the arrangement at the same level
plot(fit,hang=-1)  
#cluster dendogram also showing that 3 clusters would be appropriate - exactly the same as dimensionally reduced data.

#thus this shows that we obtain the same number of clusters with the original data & the dimensionally reduced data
