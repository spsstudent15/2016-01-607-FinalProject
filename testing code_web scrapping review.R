# Final Project

# Scrapped Review from NYT given URL

library(RCurl)
library(XML)

# Extract review from NYT given URL

movie_url1 <- "http://www.nytimes.com/2016/04/22/movies/review-the-huntsman-a-study-in-hollywoods-overstuffed-playbook.html"
movie_url2 <- "http://www.nytimes.com/glogin?URI=http%3A%2F%2Fwww.nytimes.com%2F2016%2F04%2F22%2Fmovies%2Freview-the-huntsman-a-study-in-hollywoods-overstuffed-playbook.html%3F_r%3D0"
movie_url3 <- "http://www.nytimes.com/2016/04/22/movies/review-the-huntsman-a-study-in-hollywoods-overstuffed-playbook.html?_r=0"

nyt_review1 <- getURL(movie_url1, ssl.verifypeer = FALSE, verbose = TRUE)
nyt_review2<- getURL(movie_url2, cookie = "RMID=007f01011d7157216e66000f;Path=/; Domain=.nytimes.com;Expires=Fri, 28 Apr 2017 01:59:02 UTC", ssl.verifypeer = FALSE, verbose = TRUE)
nyt_review3 <- getURL(movie_url3, cookie = "NYT-S=0M1BzC7Pk/z./DXrmvxADeHEk8LWXVZ2g0deFz9JchiAIUFL2BEX5FWcV.Ynx4rkFI; expires=Sat, 28-May-2016 02:01:06 GMT; path=/; domain=.nytimes.com",
                      ssl.verifypeer = FALSE, verbose = TRUE)

#############################################################


content_review = htmlTreeParse(nyt_review3, asText=TRUE, useInternalNodes = TRUE, encoding = 'UTF-8')

plain.text <- xpathSApply(xmlRoot(content_review), "//p[@class = 'story-body-text story-content']", xmlValue)
plain.text <- cat(paste(plain.text, collapse = "\n"))


class(content_review)

