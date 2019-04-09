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
library(gridExtra)
library(recosystem)
library(Hmisc)


#________________________________________________#
############                          ############
############    Preparing the data    ############
############                          ############
#________________________________________________#

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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

save_edx <- edx
save_valid <- validation


#________________________________________________#
############                          ############
############    Processing the data   ############
############                          ############
#________________________________________________#

processDataset <- function(df){
  df$userId <- as.factor(df$userId) 
  df$movieId <- as.factor(df$movieId) 
  df$genres <- as.factor(df$genres) 
  df$timestamp <- as.POSIXct(df$timestamp, origin = "1970-01-01")
  
  df <- df %>% 
    mutate(title = str_trim(title), year_rate = year(timestamp)) %>% # remove whitespaces from title and extract the year from the timestamp
    extract(title, c("title_tmp", "year"), # separate the title and the relase year
            regex = "^(.*) \\(([0-9 \\-]*)\\)$",
            remove = F) %>%
    # Deal with NAs and errors
    mutate(year = if_else(str_length(year) > 4,
                          as.integer(str_split(year, "-",
                                               simplify = T)[1]),
                          as.integer(year))) %>%
    mutate(title = if_else(is.na(title_tmp), title, title_tmp), age = year_rate - year) %>%
    select(-title_tmp)  %>%
    mutate(genres = if_else(genres == "(no genres listed)",
                            `is.na<-`(genres), genres)) %>% 
    select(-title, -timestamp)
  
  # check for outliers:
  numvectors <- data.frame(df$rating,df$year, df$year_rate)
  summary(numvectors)
  return(df)
}

# Applying the processing to the training set and the validation set

edx <- processDataset(edx)
validation <- processDataset(validation)

#________________________________________________#
############                          ############
############    Data exploration      ############
############                          ############
#________________________________________________#

# number of observations and variables:
dim(edx)
#number of uniques users
n_distinct(edx$userId)
# number of uniques movies
n_distinct(edx$movieId)
# number of distinct genres| genres can be a combination of many so we split them to count:
n_distinct(edx %>% separate_rows(genres, sep= "\\|") %>% select(genres))

# Distribution of ratings:
mean = mean(edx$rating)
stdv = sd(edx$rating)
edx %>% ggplot(aes(x=rating)) + geom_histogram(binwidth=0.5, colour="blue", 
                                              aes(y=..density.., fill=..count..)) +
                      scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
                      stat_function(fun=dnorm, color="red",args=list(mean, stdv))
                         
# Frequency of ratings by release date:
edx %>% ggplot(aes(year)) +
  geom_histogram(fill = "black") +
  labs(title = "Frequency of ratings by relase year",
       x = "Year",
       y = "Frequency")
                          
# Discover any pattern within the age:
edx <- edx %>% mutate(age = year_rate - year)

plot1 <- edx %>% ggplot(aes(age)) +
  geom_histogram(binwidth = 5, fill = "black") +
  labs(title = "Frequency of ratings by age of the movie",
       x = "Age",
       y = "Frequency")

plot2 <- edx %>% 
  group_by(age) %>% 
  summarize(b_a = mean(rating)) %>% 
  ggplot(aes(b_a)) + 
  geom_histogram(color = "darkblue") +
  ggtitle("Mean rating by age of the movie")
  
grid.arrange(plot1, plot2, ncol=2)

# Analyse the users and movie effects on ratings
plot1 <- edx %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Rating count by users")
plot2 <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "darkblue") +
  ggtitle("Mean rating by users")
plot3 <- edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Rating count by movies")
plot4 <- edx %>% 
  group_by(userId) %>% 
  summarize(b_m = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_m)) + 
  geom_histogram(bins = 30, color = "darkblue") +
  ggtitle("Mean rating by movies")
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)

# Analysing the impact of volume on ratings:
edx %>%
  select(genres, rating) %>%
  group_by(genres) %>%
  summarize(mean = mean(rating), median = median(rating), n = n()) %>%
  arrange(desc(mean)) %>%
  head(10)

#________________________________________________#
############                          ############
############    Model Construction    ############
############                          ############
#________________________________________________#

#############################################################
# A first Naive model
#############################################################

#Predicting the average for all users and movies
mu_hat <- mean(edx$rating)
naive_rmse <- rmse(validation$rating, mu_hat)
naive_rmse
# Creating a dataframe containing the rmse results of all methods
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#############################################################
# Modeling movie effects
#############################################################

#first, estimate the effect of each movie bi
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# then, make the prediction using the new model y= mu + b
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- rmse(predicted_ratings, validation$rating)
model_1_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))


#############################################################
# Modeling user effects
#############################################################

#first, estimate the effect of each movie bi
mu <- mean(edx$rating) 
user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))
# then, make the prediction using the new model y= mu + b
predicted_ratings <- mu + validation %>% 
  left_join(user_avgs, by='userId') %>%
  .$b_u
model_2_rmse <- rmse(predicted_ratings, validation$rating)
model_2_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="User Effect Model",  
                                     RMSE = model_2_rmse ))



#############################################################
# Modeling movie and user effects
#############################################################

# Let's compute the new term bu in our mode y= mu+bi+bu +eps
mvuser_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_a = mean(rating - mu - b_i))
# Now, compute the predicted ratings using the new model with user and movie effects:
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(mvuser_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_a) %>%
  .$pred
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
model_3_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse ))

#############################################################
# Modeling movie, user and age effects
#############################################################

# Let's compute the new term bu in our mode y= mu+bi+ba+bg +eps
age_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(mvuser_avgs, by='userId') %>%
  group_by(age) %>%
  summarize(b_g = mean(rating - mu - b_i - b_a))

# process the variable age so that leftjoin finds values in LHS
listofages <- unique(age_avgs$age)
ages <- validation$age
validation$age <- sapply(ages, function(x){ listofages[which.min(abs(listofages-x))]})
# Now, compute the predicted ratings using the new model with user and movie effects:
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(mvuser_avgs, by='userId') %>%
  left_join(age_avgs, by='age') %>%
  mutate(pred = mu + b_i + b_a + b_g) %>%
  .$pred
model_4_rmse <- RMSE(predicted_ratings, validation$rating)
model_4_rmse
# Add the rmse result of the new model to the rmse data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Age Effects Model",  
                                     RMSE = model_4_rmse ))


#############################################################
# Introducing regularization to improve the scores
#############################################################

# Printing the best rated movies in descending order and their count

movie_titles <- save_edx %>% 
  select(movieId, title) %>%
  distinct()

save_edx %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)
 
# Using regularization and tuning the penalty term

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_a <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_a = sum(rating - b_i - mu)/(n()+l))
  

  
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_a, by = "userId") %>%
    
    mutate(pred = mu + b_i + b_a  ) %>%
    .$pred
  
  return(rmse(predicted_ratings, validation$rating))
})

# Plot the values or RMSE against lambdas
qplot(lambdas, rmses)
# Find the value of lambda that minimizes the RMSE
lambdas[which.min(rmses)]
min(rmses)
# save the model with the minimal RMSE to the results data frame

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User   Effect Model",  
                                     RMSE = min(rmses)))

#________________________________________________#
############                          ############
############    Reporting results     ############
############                          ############
#________________________________________________#

format.df(rmse_results,6)
