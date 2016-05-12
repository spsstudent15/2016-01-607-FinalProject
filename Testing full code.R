####################################################################
#NY Time Review extract and Sentiment Analysis                     #
#                                                                  #
####################################################################

# For each URl in combo

library(RCurl)
library(XML)
library(dplyr)
library(indicoio)

# indicoio_API_Key <- '205a5d08430642442b316db728c26a94'
my_indicoio_API_Key <-"2cf9508e4628b67c9b69ae7d1059efda"

# Load the list of 40 movies review URL's
#movie_list_url <- getURL("https://raw.githubusercontent.com/spsstudent15/Data607FinalProject/master/nytmovies2.csv")

#
#list <- read.csv(textConnection(movie_list_url), header = TRUE, sep = ",")

movie_list <- read.csv(file = "movielist.csv", header = TRUE, sep = ",")

movie_list[1:5,]
str(movie_list)

# Set Up xpath to retrieve review and genre
xpath_review <- "//p[@class = 'story-body-text story-content']"
xpath_genre <- "(//span[@itemprop='genre'][@class = 'genre'])[1]" # will only retrieve first one

agent <- c(R.version$version.string, ",", R.version$platform)

# create data.frame to score results
movie_score <- data.frame()

# Loop through movie list, retrieve review for each movie review url
loop_counter <- nrow(movie_list)

for (i in 1:loop_counter){
  
  #Set RCurl pars
  curl <- getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl)
  
  movie_url <-  movie_list$url[i]
  
  # Get Review corresponding the url
  my_movie <- getURL(as.character(movie_url), curl = curl, verbose = FALSE)
  
  content_review = htmlTreeParse(my_movie, asText=TRUE, useInternalNodes = TRUE, encoding = 'UTF-8')
  
  plain_text <- xpathSApply(xmlRoot(content_review), xpath_review, xmlValue)
  review_df <- as.data.frame(as.character(paste(plain_text, collapse = " ")))
  
  colnames(review_df) <- "review"
  review_df$movie <- movie_list$movie[i]
  
  review_df$Sentiment_Score_nyt <- unlist(sentiment(as.character(review_df$review), api_key = my_indicoio_API_Key))
  
  genre <- xpathSApply(xmlRoot(content_review), xpath_genre, xmlValue)
  review_df$genre <-genre
  
  movie_score <- rbind(movie_score, review_df)
}

str(movie_score)

movie_df <- inner_join(movie_list, movie_score, by = "movie")


write.csv(movie_df, file = "movie_df.csv", sep = ",")    

    
####################################################################
# Retrieve Tweets
####################################################################

tweets_url <- getURL("https://raw.githubusercontent.com/spsstudent15/Data607FinalProject/master/tweets503to509.csv")
tweets_list <- read.csv(text = tweets_url, header = TRUE, sep = ",")

movie_df <- read.csv("movie_df.csv", header = TRUE, sep = ",")
str(tweets_list)
colnames(movie_df)[9] <- "Sentiment_Score_nyt"


nrow(movie_df)
nrow(tweets_list)

my_data_raw <- inner_join(tweets_list, movie_df, by = "movie")
nrow(my_data_raw)

###########################
# Fix Los Angeles

my_data_raw$city[my_data_raw$city == 'los Angeles'] <- 'Los Angeles'

str(my_data_raw)

#################
# NYT reviews and Sentiment Analysis that went wrong..... 
nyt_false_positive <- movie_df %>% filter(Sentiment_Score_nyt < 0.75 & critics_pick == 1) %>% 
                    select(movie, mpaa_rating, genre, Sentiment_Score_nyt, critics_pick)

nyt2 <- movie_df %>% filter(Sentiment_Score_nyt >= 0.80 & critics_pick == 0) %>% 
                     select(movie, mpaa_rating, genre, Sentiment_Score_nyt, critics_pick)
 
nyt3 <- movie_df %>% filter(Sentiment_Score_nyt < 0.80 & critics_pick == 0) %>% 
        select(movie, mpaa_rating, genre, Sentiment_Score_nyt, critics_pick)

nyt_false_positive

### Selecting columns

my_data_analysis <- select (my_data_raw, movie, day, opening_date, publication_date, 
                            mpaa_rating, genre, critics_pick, city, Sentiment_Score, Sentiment_Score_nyt)


my_data_analysis %>% ungroup()

data_set_movie2 <- my_data_analysis %>%  group_by (movie, city, genre, critics_pick) %>% 
                  summarise(mean_score = mean(Sentiment_Score), mean_score_nyt = mean(Sentiment_Score_nyt), count = n())

filter (my_data_raw, city == "los Angeles")
#### Filter out any rows where count < 15

data_set_movie <- my_data_analysis %>%  group_by (movie, genre, critics_pick) %>% 
                  summarise (mean_score = mean(Sentiment_Score), mean_score_nyt = mean(Sentiment_Score_nyt), count = n())


data_set_movie_reduce <- data_set_movie %>% filter(count>= 15)


data_set_movie_reduce

library(ggplot2)
library(IS606)

summary(data_set_movie_reduce)

ggplot(data_set_movie_reduce, aes(x=mean_score_nyt, y=mean_score)) + geom_boxplot()

ggplot(data_set_movie_reduce, aes(x=mean_score_nyt, y=mean_score)) + geom_point()

#plot_ss(x = data_set_movie_reduce$mean_score_nyt, y = data_set_movie_reduce$mean_score, showSquares = TRUE)

m1 <- lm(mean_score ~ mean_score_nyt, data = data_set_movie_reduce)

summary(m1)

hist(m1$residuals)

qqnorm(m1$residuals)
qqline(m1$residuals)

######################################################

ggplot(data_set_movie_reduce, aes(x=mean_score_nyt, y=reorder(movie, mean_score_nyt))) + geom_point(size = 2)

ggplot(data_set_movie_reduce, aes(x=mean_score_nyt, y=mean_score, colour=genre)) + geom_line()

