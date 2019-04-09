library(caret)
library(dplyr)
library(tidyverse)
library(lubridate)
library("e1071")
library("genefilter")
library(dslabs)
library(rpart)
library(rpart.plot)
library(randomForest)
library(Rborist)
library(purrr)
library(Metrics)
library(RColorBrewer)
library(matrixStats)
library(stringr)

#############################################################
# Introduction - Creating data sets
#############################################################
data("movielens")
movielens <- movielens %>% mutate(wk = round_date(as_datetime(timestamp), unit = "week"))
head(movielens)

# check the number of distinct users and movies
movielens %>% summarise(users =n_distinct(userId), movies= n_distinct(movieId))

# See the distribution of movies rating
movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")
# See the distribution of users rating activity
movielens %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")
# Create train and test data set using 10% index
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
# Make sure userId and movieId exist in the two data sets
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "wk") %>%
  semi_join(train_set, by = "genres")

#############################################################
# A first Naive model
#############################################################

#Predicting the average for all users and movies
mu_hat <- mean(train_set$rating)
naive_rmse <- rmse(test_set$rating, mu_hat)
naive_rmse
# Creating a dataframe containing the rmse results of all methods
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#############################################################
# Modeling movie effects
#############################################################

#first, estimate the effect of each movie bi
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
# then, make the prediction using the new model y= mu + b
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- rmse(predicted_ratings, test_set$rating)
model_1_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))

#############################################################
# Modeling user effects
#############################################################

# See the distribution of average rating of users with more than 100 ratings
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# Let's compute the new term bu in our mode y= mu+bi+bu +eps
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
# Now, compute the predicted ratings using the new model with user and movie effects:
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

#############################################################
# Using Regularisation to account for small sets of ratings
#############################################################

lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
movie_reg_avgs
# Now, compute the predicted ratings using the new model with movie effect and regularisation
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- rmse(predicted_ratings, test_set$rating)
model_3_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))

#############################################################
# Choosing Lambda for regularisation and including both effects
#############################################################

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_t <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(wk) %>%
    summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_t, by="wk") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - b_t - mu)/(n()+l))
 
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "wk") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
    .$pred
  
  return(rmse(predicted_ratings, test_set$rating))
})

# Plot the values or RMSE against lambdas
qplot(lambdas, rmses)
# Find the value of lambda that minimizes the RMSE
lambdas[which.min(rmses)]
min(rmses)
# save the model with the minimal RMSE to the results data frame

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + time + genre Effect Model",  
                                     RMSE = min(rmses)))


