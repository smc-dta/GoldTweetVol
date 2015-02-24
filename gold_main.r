######################################################################################
#Gold Tweet Volumes and Price Returns
######################################################################################

library(dplyr)
library(ggplot2)
library(xts)
library(sandwich)
library(lmtest)

#Read and sort tweets
source("processTweets.r")

#Read & plot gold price data; calculate daily price return & volatility
source("processPriceDate.r")

#Tweets occuring on days when the market is closed are grouped into the variable ¨wtw¨
#and the date is adjusted to the next date when the market is open
source("setTweetDate.r")

#Examines correlations between daily returns and tweet vols, and daily volatility and
#tweet vols
source("ReturnsAndTweetVol.r")



