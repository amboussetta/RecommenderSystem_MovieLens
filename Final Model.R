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


#________________________________________________#
############                          ############
############    Processing the data   ############
############                          ############
#________________________________________________#

edx$userId <- as.factor(edx$userId) 
edx$movieId <- as.factor(edx$movieId) 
edx$genres <- as.factor(edx$genres) 
edx$timestamp <- as.POSIXct(edx$timestamp, origin = "1970-01-01")

edx <- edx %>% 
  mutate(title = str_trim(title), year_rate = year(timestamp)) %>% # remove whitespaces from title and extract the year from the timestamp
  extract(title, c("title_tmp", "year"), # separate the title and the relase year
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  # Deal with NAs and errors
  mutate(year = if_else(str_length(year) > 4,
                        as.integer(str_split(year, "-",
                                             simplify = T)[1]),
                        as.integer(year))) %>%
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  select(-title_tmp)  %>%
  mutate(genres = if_else(genres == "(no genres listed)",
                          `is.na<-`(genres), genres)) %>% 
    select(-title, -timestamp)

# check for outliers:
numvectors <- data.frame(edx$rating,edx$year, edx$year_rate)
summary(numvectors)

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