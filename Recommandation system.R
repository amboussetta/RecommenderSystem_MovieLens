library(caret)
library(dplyr)
library(tidyverse)
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
# Create edx set, validation set, and submission file
#############################################################

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
movielens <- movielens %>% mutate(wk = round_date(as_datetime(timestamp), unit = "week"))

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId movieId wk and genres in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>%
  semi_join(edx, by = "wk") %>%
  semi_join(edx, by = "genres")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#############################################################
# Edx Dataset analysis
#############################################################

head(edx)
nrow(edx)
ncol(edx)
sum(edx$rating==0)
sum(edx$rating==3)
length(unique(edx$movieId))
length(unique(edx$userId))
edx_genres<-edx %>% mutate(drama=ifelse(grepl('Drama',genres),1,0),comedy=ifelse(grepl('Comedy',genres),1,0),romance=ifelse(grepl('Romance',genres),1,0),thriller=ifelse(grepl('Thriller',genres),1,0))
sapply(c(7:10),function(n){sum(edx_genres[,n])})
bestmovies<-edx %>% group_by(movieId) %>% summarise(nrat=n()) %>% arrange(desc(nrat)) %>% slice(1:10)
edx %>% select(movieId,title) %>% filter(movieId==296)
edx %>% group_by(rating) %>% summarise(count=n()) %>% arrange(desc(count))
edx %>% mutate(isNat=ifelse(rating%%1==0,1,0)) %>% summarise(mean(isNat))
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

head(edx)

nrwo(edx)
nrow(edx)
nrow(validation)

#############################################################
# Recommander system using movie, user, time and genre effect
#############################################################

mu <- mean(edx$rating)
l<-5.5
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_t <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(wk) %>%
  summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+l))

b_g <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_t, by="wk") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - b_t - mu)/(n()+l))


predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "wk") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
  .$pred

rmse(predicted_ratings,validation$rating)