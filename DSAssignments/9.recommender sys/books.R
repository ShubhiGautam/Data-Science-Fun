#importing the dataset
library(readr)
data<-read_csv("C://Users/Rajesh/Documents/me/data_assignments/mod16/book.csv")
head(data)
View(data)
data<-data[,-1]
attach(data)


## EDA
dim(data)
str(data) #need the data to be in matrix format so change it to dataframe first
data <- as.data.frame(data)
str(data)
colnames(data)
summary(data)
class(data)


#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#just to see the hist rating distrubution
hist(data$`ratings[, 3]`) # most ratings are 0

#data type needs to be realRatingMatrix  in order to build the recommendation engine
movie_rate_data_matrix <- as(data, 'realRatingMatrix')
movie_rate_data_matrix

#Popularity based 
movie_recomm_model1 <- Recommender(movie_rate_data_matrix, method="POPULAR")
View(movie_recomm_model1)

#recommend 3 books to user with that id 
recommended_items1 <- predict(movie_recomm_model1, movie_rate_data_matrix[977], n=3)
as(recommended_items1, "list")

# Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering (UBCF) (user to user based)

movie_recomm_model2 <- Recommender(movie_rate_data_matrix, method="UBCF")

#recommend 5 books with user with that id
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix[33], n=2)
as(recommended_items2, "list")


