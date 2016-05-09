#required libraries
library(jsonlite)
library(knitr)

#RESULTS 0-20 AS OF 2016-05-01
NYTREVIEWS_JSON_URL1 = 'http://api.nytimes.com/svc/movies/v2/reviews/search.json?opening-date=2016-04-21;2016-05-01&api-key=971b11ea372d3abc7c9bb7d914767aad:0:65677168'#url for api results 0-20
json_file1 <- fromJSON(NYTREVIEWS_JSON_URL1)# get json
df1 <- as.data.frame(json_file1$results) #dataframe for results
#str(df1) #view structure 
#colnames(df1) #view column names
df1s<-df1[,c(1:8)] #subset needed columns

#RESULTS 21-40 AS OF 2016-05-01
NYTREVIEWS_JSON_URL2 = 'http://api.nytimes.com/svc/movies/v2/reviews/search.json?opening-date=2016-04-21;2016-05-01&api-key=971b11ea372d3abc7c9bb7d914767aad:0:65677168&offset=20' #url for api results 20-40
json_file2 <- fromJSON(NYTREVIEWS_JSON_URL2) #get json
df2 <- as.data.frame(json_file2$results) #dataframe for results
df2s<-df2[,c(1:8)] #subset needed columns

#ALL RESULTS 0-40 COMBINED
df<- rbind(df1s,df2s) #combine all results
#knitr::kable(df, row.names =TRUE) #test

#ADD URL COLUMN TO RESULTS
url1 <- json_file1$results$link #collect links part 1
url1<-url1[,c(1:2)] #subset links
url2 <- json_file2$results$link #collect links part 2
url2<-url2[,c(1:2)] #subset links
#str(url1) #test
urls<- rbind(url1,url2) #combine all results for links
#knitr::kable(urls, row.names = TRUE) #test
combo<-cbind(df,urls) #combine reviews and links
combo<-combo[,c(1:3,7,8,10)] #subset needed columns
combo<-combo[,c(1,5,4,6,2,3)] #reorder needed columns
kable(combo, row.names = TRUE) #display with row numbers

####################################################################

# For each URl in combo

library(RCurl)
library(XML)
library(indicoio)
# 
# indicoio_API_Key <- '205a5d08430642442b316db728c26a94'
my_indicoio_API_Key <-"2cf9508e4628b67c9b69ae7d1059efda"

movie_list_url <- "https://raw.githubusercontent.com/spsstudent15/Data607FinalProject/master/20160508nytreviews.csv"
movie_list <-read.csv(movie_list_url, header = TRUE, sep = "")

movie_list[1:5,]

movie_review_text <- list()

agent <- c(R.version$version.string, ",", R.version$platform)

#loop_counter <-length(combo$url
loop_counter <- nrow(movie_list)
for (i in 1:loop_counter){
  
  #Set RCurl pars
  curl <- getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl)
  
  movie_url <-  movie_list$url[i]
  
  # Get Review corresponding the url
  my_movie <- getURL(as.character(movie_url), curl = curl, verbose = TRUE)
  
  content_review = htmlTreeParse(my_movie, asText=TRUE, useInternalNodes = TRUE, encoding = 'UTF-8')
  
  plain_text <- xpathSApply(xmlRoot(content_review), "//p[@class = 'story-body-text story-content']", xmlValue)
  plain_text <- paste(plain_text, collapse = "\n")
  
  movie_review_text[[i]] <- plain_text
}

df_review <- as.data.frame(unlist(movie_review_text))


names(df_review) <- c("reviews") 

movie_df <- cbind(movie_list, df_review)

write.csv(movie_df, file = "movie_df.csv", sep = ",")    
    
str(movie_df)    

movie_df$review_score <- apply(movie_df[,c('reviews')], 1, function(x){sentiment((rm_special(as.character(x))), api_key = my_indicoio_API_Key)})


companiesData$margin <- apply(companiesData[,c('revenue', 'profit')], 1, function(x) { (x[2]/x[1]) * 100 } )










    
##################################################################################
#  unlist(sentiment(as.character(rm_special(plaint.text)), 
#                   api_key = indicoio_API_Key))




#################################################################################

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")


library(devtools)
devtools::install_github("IndicoDataSolutions/IndicoIo-R")