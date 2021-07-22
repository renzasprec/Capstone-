# Code provided by edx-----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# install packages and load libraries
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(knitr)
library(lubridate)
library(tinytex)
library(recosystem)

# options
options(digits=5)

# Initial exploration of the data----
as_tibble(edx)

# no of unique users
n_users<- edx %>% select(userId) %>%
  distinct() %>%
  summarize(n=n())

# list of genres
l_genres<- edx %>% separate_rows(genres, sep="\\|") %>%
  select(genres) %>%
  distinct()
l_genres %>% kable()

# no of genres
l_genres %>% summarize(n=n())

# distribution of ratings
edx %>%
  select(rating) %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth=0.5, color="black")

# convert timestamp to date format
edx_date<- edx %>%
  mutate(date=as_datetime(timestamp)) %>%
  select(-timestamp)

# year range of the ratings
year(range(edx_date$date))

# create evaluation metrics, RMSE
RMSE<- function(rating_true, rating_predicted){
  sqrt(mean((rating_true-rating_predicted)^2))
}
# Naive Approach - Prediction of rating based on the average of all ratings ---------------
# compute the average of all ratings
mu<- mean(edx$rating)

# perform rating prediction based on the average of all ratings
rating_predicted<- mu

# resulting rmse
rmse<- RMSE(validation$rating,rating_predicted)

# table of rmses
rmses_results<- data.frame(model= "Naive Approach: mean of all ratings", RMSE= rmse) 


# Movie bias - Prediction of rating based on the average ratings and a movie bias, b_i-----
# compute the average of all ratings
mu<- mean(edx$rating)

# compute b_i by averaging the residuals for each movie
movie_averages<- edx %>%
  group_by(movieId) %>%
  summarize(b_i= mean(rating- mu))

# plot of b_i
qplot(movie_averages$b_i, bins=10, color=I("black"), xlab="b_i")

# predict rating taking into account the movie bias, b_i
rating_predicted<- validation %>% 
  left_join(movie_averages, by="movieId") %>%
  mutate(prediction= mu + b_i) %>%
  pull(prediction)

# compute the RMSE taking into account the movie bias, b_i
rmse<- RMSE(validation$rating, rating_predicted)
rmses_results<- bind_rows(rmses_results,
                          data.frame(model="Mean with movie bias", RMSE= rmse))

# User-bias - Prediction of rating based on the average ratings, a movie bias, b_i, and a user bias, b_u----

# compute average for all ratings
mu<- mean(edx$rating)

# compute b_u by averaging the residuals(including the bias on movies) for each user
user_averages<- edx %>%
  left_join(movie_averages, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating-mu-b_i))

# plot of b_u
qplot(user_averages$b_u, bins=10, color=I("black"), xlab="b_u")

# predict ratings taking into account movie bias, b_i, and user bias, b_u
rating_predicted<- validation %>%
  left_join(movie_averages, by="movieId") %>%
  left_join(user_averages, by="userId") %>%
  summarize(prediction = mu+b_i+b_u) %>% 
  pull(prediction)

# compute RMSE 
rmse<- RMSE(validation$rating, rating_predicted)
rmses_results<- bind_rows(rmses_results,
                          data.frame(model="Mean with movie bias and user bias", RMSE=rmse))

# Genre-bias Approach - Prediction of rating based on a movie-bias, b_i, user-bias, b_u, and genre-bias, b_u_g -----

# compute average for all ratings
mu<- mean(edx$rating)

# compute b_u_g by averaging the residuals (including the movie-bias, and user-bias)
genre_averages<- edx %>%
  left_join(movie_averages, by="movieId") %>%
  left_join(user_averages, by="userId") %>%
  group_by(genres) %>%
  summarize(b_u_g = mean(rating-mu-b_i-b_u))

# plot of b_u_g
qplot(genre_averages$b_u_g, bins=10, color=I("black"), xlab="b_u_g")

# predict ratings taking into account the movie bias, b_i, user bias, b_u, and genre bias, b_u_g
rating_predicted<- validation %>%
  left_join(movie_averages, by="movieId") %>%
  left_join(user_averages, by="userId") %>%
  left_join(genre_averages, by="genres") %>%
  summarize(prediction = mu+ b_i+b_u+b_u_g) %>%
  pull(prediction)

# compute RMSE taking
rmse<- RMSE(validation$rating, rating_predicted)
rmses_results<- bind_rows(rmses_results, data.frame(model="Mean with movie bias, user bias, and genre bias", RMSE=rmse))
  
# Regularized Movie bias Approach - Penalize large estimates of b_i formed from small sample sizes------

# mean of ratings from edx
mu<- mean(edx$rating)

# output movie titles
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

# output top and bottom 10 movies according to estimate
top_movies<- movie_averages %>% 
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

bottom_movies<- movie_averages %>% 
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

# investigate number of ratings for the movies in top and bottom 10
top_10_count<- edx %>% 
  filter(title %in% top_movies) %>%
  group_by(title) %>%
  summarize(n=n()) %>%
  arrange(n)

bottom_10_count<- edx %>% filter(title %in% bottom_movies) %>%
  group_by(title) %>%
  summarize(n=n()) %>%
  arrange(n)

# output top and bottom 10 movies
top_10_count %>% kable()
bottom_10_count %>% kable()

# optimize the penalty, lambda (using only the training set, edx)
# cross-validation using 20% as test data. Create a train set and a test set based on the edx data, for the optimization of lambdas. Note: the validation set must not be used for the optimization of lambdas

ind<- createDataPartition(edx$rating, times=1, p=0.2, list=FALSE)
temp<- edx[ind]
train_set<- edx[-ind]

# make sure that all movieId, and userId in the test_set are in the train_set
test_set <- temp %>% 
  semi_join(train_set, by="movieId") %>%
  semi_join(train_set, by="userId")
  
# Add rows removed from test_set back into train_set set
removed<- anti_join(temp, test_set)
train_set<- bind_rows(train_set,removed)

# Dimensions of training and testing set
dim(test_set)
dim(train_set)

# range of lambdas used for optimization
lambdas<- seq(0,10,0.1)

# compute average, mu, for the train_set
mu<- mean(train_set$rating)

# function to compute for the rmses for the assumed lambdas
rmses_reg<- sapply(lambdas, function(l){
  
  # solves for the regularized_movie_averages
  movie_averages_reg<- train_set %>%
  group_by(movieId) %>%
  summarize(b_i_reg = sum(rating-mu)/(n()+l), n_i=n())
  
  # predict rating based on b_i_reg
  rating_predicted_reg<- test_set %>%
    left_join(movie_averages_reg, by="movieId") %>%
    summarize(prediction = mu+b_i_reg) %>%
    pull(prediction)
  
  # compute for rmse
  RMSE(test_set$rating, rating_predicted_reg)
})

# get lambda which provides the minimum rmse
lambda<- lambdas[which.min(rmses_reg)]

# plot rmse vs lambda
qplot(lambdas,rmses_reg)

#recompute average, mu, for the edx set
mu<- mean(edx$rating)

# compute regularized movie bias, b_i_reg
movie_averages_reg<- edx %>%
  group_by(movieId) %>%
  summarize(b_i_reg= sum(rating-mu)/(n()+lambda), n_i=n())

# predict rating based on b_i_reg
rating_predicted_reg<- validation %>%
  left_join(movie_averages_reg, by="movieId") %>%
  summarize(prediction = mu+b_i_reg) %>%
  pull(prediction)

# compute for RMSE
rmse<- RMSE(validation$rating, rating_predicted_reg)
rmses_results<- bind_rows(rmses_results,
                          data.frame(model= "Mean with regularized movie bias", RMSE=rmse))
rmses_results %>% kable()

# Regularized User bias approach - Penalize large estimates of b_i and b_u formed from small sample sizes------

# show the bottom 10 users based on number of ratings
edx %>% group_by(userId) %>%
  summarize(count=n()) %>%
  arrange(count) %>%
  slice(1:10) %>%
  kable()

# show top 10 users based on number of ratings
edx %>% group_by(userId) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  kable()

# optimize the penalty, lambda (using only the training set, edx)

# range of lambdas used for optimization
lambdas<- seq(0,10,0.1)

# compute for regularized b_u 
mu<- mean(train_set$rating)

# compute RMSEs
rmses_reg<- sapply(lambdas, function(l){
  
  # compute for b_i_reg
  movie_averages_reg<- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating-mu)/(n()+l), n_i=n())
  
  # compute for b_u_reg
  user_averages_reg<- train_set %>%
    left_join(movie_averages_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating-mu-b_i_reg)/(n()+l))
  
  # predict ratings
  rating_predicted_reg<- test_set %>%
    left_join(movie_averages_reg, by="movieId") %>%
    left_join(user_averages_reg, by="userId") %>%
    summarize(prediction = mu+b_i_reg+b_u_reg) %>%
    pull(prediction)
  
  # compute RMSE
  RMSE(test_set$rating, rating_predicted_reg)
})

# plot lambdas vs rmses
qplot(lambdas,rmses_reg)

# obtain lambda that minimizes the rmses of the assigned test_set within the edx data
lambda<- lambdas[which.min(rmses_reg)]

# compute the RMSE of the validation set based on the optimized lambda
mu<- mean(edx$rating)

# compute for b_i_reg
movie_averages_reg<- edx %>%
  group_by(movieId) %>%
  summarize(b_i_reg = sum(rating-mu)/(n()+lambda), n_i=n())

# compute for b_u_reg
user_averages_reg<- edx %>%
  left_join(movie_averages_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_reg = sum(rating-mu-b_i_reg)/(n()+lambda)) 

# predict rating
rating_predicted_reg<- validation %>%
  left_join(movie_averages_reg, by="movieId") %>%
  left_join(user_averages_reg, by="userId") %>%
  summarize(prediction = mu+b_i_reg+b_u_reg) %>%
  pull(prediction)

# compute for RMSE based on the edx and validation data set
rmse<- RMSE(validation$rating,rating_predicted_reg)
rmses_results<- bind_rows(rmses_results,
                          data.frame(model="Mean with regularized movie and user bias", RMSE=rmse))
rmses_results %>% kable()

# Regularized genre-based approach - Penalize large estimates of b_i, b_u, and b_u_g formed from small sample sizes -----

# use training set and test set obtained from the edx data set to optimize lambda

# range of lambdas used for optimization
lambdas<- seq(0,10,0.1)

mu<- mean(train_set$rating)
rmses_reg<- sapply(lambdas, function(l){
  # compute for b_i
  movie_averages_reg<- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_reg= sum(rating-mu)/(n()+l), n_i=n())
  
  # compute for b_u
  user_averages_reg<- train_set %>%
    left_join(movie_averages_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating-mu-b_i_reg)/(n()+l))
  
  # compute for b_u_g
  genre_averages_reg<- train_set %>%
    left_join(movie_averages_reg, by="movieId") %>%
    left_join(user_averages_reg, by="userId") %>%
    group_by(genres) %>%
    summarize(b_u_g_reg= sum(rating-mu-b_i_reg-b_u_reg)/(n()+l))
  
  # prediction based on test_set obtained from the edx data set
  rating_predicted_reg<- test_set %>%
    left_join(movie_averages_reg, by="movieId") %>%
    left_join(user_averages_reg, by="userId") %>%
    left_join(genre_averages_reg, by="genres") %>%
    summarize(prediction = mu+b_i_reg+b_u_reg+b_u_g_reg) %>%
    pull(prediction)
  
  # compute for RMSE
  RMSE(test_set$rating, rating_predicted_reg)
})

# plot of the resulting RMSEs vs lambdas
qplot(lambdas, rmses_reg) 

# obtain lambda which optimizes the RMSES
lambda<- lambdas[which.min(rmses_reg)]

# compute the RMSE of the validation set based on the optimized lambda
mu<- mean(edx$rating)

# compute for b_i
movie_averages_reg<- edx %>%
  group_by(movieId) %>%
  summarize(b_i_reg = sum(rating-mu)/(n()+lambda), n_i=n())

# compute for b_u
user_averages_reg<- edx %>%
  left_join(movie_averages_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_reg = sum(rating-mu-b_i_reg)/(n()+lambda))

# compute for b_u_g
genre_averages_reg<- edx %>%
  left_join(movie_averages_reg, by="movieId") %>%
  left_join(user_averages_reg, by="userId") %>%
  group_by(genres) %>%
  summarize(b_u_g_reg = sum(rating-mu-b_i_reg-b_u_reg)/(n()+lambda))

# predict rating based on the edx and validation data set
rating_predicted_reg<- validation %>%
  left_join(movie_averages_reg, by="movieId") %>%
  left_join(user_averages_reg, by="userId") %>%
  left_join(genre_averages_reg, by="genres") %>%
  summarize(prediction= mu+b_i_reg+b_u_reg+b_u_g_reg) %>%
  pull(prediction)

# compute for RMSE
rmse<- RMSE(validation$rating, rating_predicted_reg)
rmses_results<- bind_rows(rmses_results, 
                          data.frame(model="Mean and regularized movie, user, and genre bias", RMSE=rmse))

rmses_results%>% kable()

# Matrix factorization ----
# Matrix factorization using recosystem
# Convert edx and validation sets to recosystem input format
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating     = rating))
validation_reco  <-  with(validation,  data_memory(user_index = userId,
                                                   item_index = movieId, 
                                                   rating     = rating))
# construct recommender system object
r <- Reco()

# set parameters
opts_tune<- r$tune(edx_reco)$min

# tuning model parameters
r$train(edx_reco, opts = opts_tune)

# predict ratings based on validation data
rating_predicted<- r$predict(validation_reco, out_memory())

rmse<- RMSE(validation$rating, rating_predicted)
rmses_results<- bind_rows(rmses_results,
                          data.frame(model= "Matrix Factorization", 
                                     RMSE= rmse)
)
rmses_results %>% kable

