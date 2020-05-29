#importing the dataset
library(readr)
data<-read_csv("C://Users/Rajesh/Documents/me/data_assignments/mod16/Movie.csv")
head(data)
View(data)
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
hist(data$rating) # most ratings are either around 3 or 4

#data type needs to be realRatingMatrix  in order to build the recommendation engine
movie_rate_data_matrix <- as(data, 'realRatingMatrix')
movie_rate_data_matrix

#Popularity based 
movie_recomm_model1 <- Recommender(movie_rate_data_matrix, method="POPULAR")
View(movie_recomm_model1)

#recommend 7 movies to user with that id 
recommended_items1 <- predict(movie_recomm_model1, movie_rate_data_matrix[1301], n=7)
as(recommended_items1, "list")

# Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering (UBCF) (user to user based)

movie_recomm_model2 <- Recommender(movie_rate_data_matrix, method="UBCF")

#recommend 5 movies with user with that id
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix[1301], n=5)
as(recommended_items2, "list")


