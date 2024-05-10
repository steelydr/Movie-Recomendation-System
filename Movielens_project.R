#Data Preparation and Analysis

#Install the pacman package
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")

#Load the required libraries
#If a package below is missing, p_load will automatically download it from CRAN
pacman::p_load(tidyverse, ggplot2, ggthemes, data.table, lubridate, caret, 
               knitr, scales, treemapify)
pacman::p_load(stringr)
pacman::p_load(dplyr)

#All Data for the MovieLens Dataset Will be obtained from the following sources:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Data Preparation

#Download File
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#Create the Data Frame "ratings" using fread from library(data.table)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\:\\:", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(unique(movieId)),
         title = as.character(title),
         genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

#Validation set will be 10% of MovieLens data
#Set Seed to 1
set.seed(1, sample.kind="Rounding")
test_index <-createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <-movielens[-test_index,]
temp <-movielens[test_index,]
#Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from validation set back into edx set
removed <-anti_join(temp, validation)
edx <-rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Partition Training and Test Sets:
set.seed(1, sample.kind = "Rounding")
test_index <-createDataPartition(y = edx$rating, times = 1, p = 0.1, list = F)
edx_train <-edx[-test_index,]
edx_temp <-edx[test_index,]
#Again, Confirm userId and movieId are in both the train and test sets
edx_test <-edx_temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")
#Add the Rows removed from the edx_test back into edx_train
removed <-anti_join(edx_temp, edx_test)
edx_train <-rbind(edx_train, removed)
rm(edx_temp, test_index, removed)

#Analyzing the data

edx %>% as_tibble()

#Explore the features and classes of edx while also confirming its dimensions

glimpse(edx)

#Determine the unique number of userIds, movieIds, and genres

edx %>% summarize(unique_users = length(unique(userId)),
                  unique_movies = length(unique(movieId)),
                  unique_genres = length(unique(genres)))


#Ratings
length(unique(edx$rating))

unique_ratings <-unique(edx$rating)
sort(unique_ratings)

#View a Tibble of the Ratings Distribution
edx %>% group_by(rating) %>% summarize(ratings_sum = n()) %>%
  arrange(desc(ratings_sum))

library(ggplot2)

# group by rating and count the number of ratings in each group
ratings_counts <- edx %>% group_by(rating) %>% summarize(ratings_sum = n()) %>%
  arrange(desc(ratings_sum))

# plot the bar chart
ggplot(data = ratings_counts, aes(x = rating, y = ratings_sum)) +
  geom_col(fill = "red") +
  labs(title = "Distribution of Ratings", x = "Ratings awarded", y = "Total Count")

rp <-edx %>% filter(edx$rating >=3)
nrow(rp)/length(edx$rating)

#Timestamp
#Convert the timestamp to “RatingYear” for the edx dataset
library(lubridate)

# Check column names in edx data frame
names(edx)

# Convert timestamp to character format
edx$timestamp <- as.character(edx$timestamp)

# Convert timestamp to POSIXct format
edx <- edx %>% 
  mutate(timestamp = ymd_hms(timestamp, tz = "EST"))

edx <- edx %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", 
                                             tz = "EST"))
edx$timestamp <- format(edx$timestamp, "%Y")
names(edx)[names(edx) == "timestamp"] <- "RatingYear"
head(edx)

#Validation

validation <- validation %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", 
                                                           tz = "EST"))
validation$timestamp <- format(validation$timestamp, "%Y")
names(validation)[names(validation) == "timestamp"] <- "RatingYear"
head(validation)

#This process is also applied to the edx_train & edx_test sets for later modeling purposes.

# Check class of timestamp variable
class(edx_train$timestamp)

# Convert timestamp to character and then to POSIXct
edx_train$timestamp <- as.character(edx_train$timestamp)
edx_train$timestamp <- as.POSIXct(edx_train$timestamp, origin = "1970-01-01", tz = "EST")
head(edx_train)



# Check class of timestamp variable edx_test
class(edx_test$timestamp)
edx_test <-edx_test %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", 
                                                      tz = "EST"))
edx_test$timestamp <- format(edx_test$timestamp, "%Y")
names(edx_test)[names(edx_test) == "timestamp"] <- "RatingYear"
head(edx_test)
#A quick check of the range tells us all ratings have taken place between 1995 & 2009.
range(edx$RatingYear)

range(edx$RatingYear, na.rm = TRUE)


#Coerce RatingYear from character to numeric to plot the histogram
edx$RatingYear <-as.numeric(edx$RatingYear)
str(edx)

na.omit(edx)

library(ggplot2)
#The histogram of RatingYear shows in which years most ratings occurred.
ggplot(edx_train, aes(x = as.numeric(RatingYear))) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  ggtitle("Histogram of RatingYear") +
  xlab("Rating Year") +
  ylab("Count")

#A tibble of the top 50 movies with the highest number of ratings reveals the vast majority were hits at the box office like “Batman” & “Forrest Gump.”
edx_train %>%
  group_by(RatingYear) %>% 
  summarize(Ratings_Sum = n(), Average_Rating = mean(rating)) %>%
  mutate(Average_Rating = sprintf("%0.2f", Average_Rating)) %>%
  arrange(-Ratings_Sum) %>% print(n = 50)

#Genres
library(tidyr)
edx_genres <-edx %>% separate_rows(genres, sep = "\\|")






