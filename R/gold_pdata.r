library(xts)
library(dplyr)
library(ggplot2)


##read gold price data
cc <- c("character", "numeric")
p <- read.table("goldTweets/data/goldPriceData.csv", header = TRUE, sep=",", quote = " ",
                colClasses = cc)
p[,1] <- as.Date(as.character(p[,1]), format = "%m/%d/%Y")
#df "p" has gold price data
p <- p[p$date > as.Date("6/28/2012", format = "%m/%d/%Y", ), ]
#remove xmas and new year from dataset.  single holidays are automatically removed, but two-day
#holidays are not
holidays <- c("2012-12-24","2012-12-25","2012-12-26","2012-12-31","2013-01-01",
                    "2013-12-24","2013-12-25","2013-12-26","2013-12-31","2014-01-01")                  
p <- p[! p$date %in% as.Date(holidays), ]

pdata <- na.omit(mutate(p, d = ((lag(p$price)-p$price)/p$price)*100)) #create daily pct change
pdata$ar <- abs(pdata$d) #absolute value of daily pct change




