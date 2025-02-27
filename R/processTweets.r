######################################################################################
#Read Tweet Files; Filter for Gold Tweets 
######################################################################################

###Read in list of influential tweeters
tweeters <- read.csv("data/inflist.csv", header = TRUE)

###Read files in data folder, filter for author and gold words
filenames <- list.files("tweets", pattern = "*.csv", full.names = TRUE)
goldtweets <- NULL  #This object will hold all the gold tweets
for(i in 1:length(filenames)){
      t <- read.csv(filenames[i], header = T, comment.char = "") 
      t <- t[as.character(t$user.screen_name) %in% 
                         as.character(tweeters$author) == TRUE,]
      gold <- t[grepl(
        " (\\$)*gold | (\\$)*gold$|^(\\$)*gold |^(\\$)gold$| (\\$)*gld |
        (\\$)*gld$|^(\\$)*gld |^(\\$)*gld$|precious metal(s)*| (\\$)*iau |
        (\\$)*iau$|^(\\$)*iau |^(\\$)*iau$",
        t$text, ignore.case = TRUE, perl = FALSE, fixed = FALSE, 
        useBytes = FALSE) == TRUE, 1:4 ]
      goldtweets <- as.data.frame(rbind(goldtweets, gold))
    }

###Calculate tweets per day, put in data.frame "gt"
goldtweets$date <- substr(goldtweets$created_at, 1, 10)
gt <- goldtweets %>% group_by(date)
gt <- na.omit(gt %>% summarise(n = n()))
gt$date <- as.Date(gt$date)

###Plot daily volume of tweets about gold
plot(gt$date, gt$n, type = "l", col = "red", main = "Daily Volume of Gold Tweets", 
     xlab = " ", ylab = "n")


