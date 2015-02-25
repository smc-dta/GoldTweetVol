######################################################################################
#Daily Returns and Tweet Volumes    
######################################################################################

###Examines correlations between daily returns and tweet vols, and daily volatility and
##tweet vols

##combine tweet vols and daily returns
data <- merge(pdata, gt, by = "date", all = TRUE)
##plots daily tweet volumes vs. daily returns
plot(data$d, data$tw, main = "Daily Gold Price Returns vs. Daily Tweet Volume", 
     xlab = "Return", ylab = "Tweets")

##plotting the section where number of tweets is less than 250
data.sub <- data[data$tw < 250, ]
plot(data.sub$d, data.sub$tw, main = "Daily Gold Price Returns vs. Daily Tweet Volume", 
     xlab = "Return", ylab = "Tweets")

##plotting daily tweet volumes vs. daily volatility
plot(data$ar, data$tw, main = "Daily Gold Price Returns vs. Daily Tweet Volatility",
     xlab = "Return", ylab = "Tweets")
##plotting the section where number of tweets is less than 250
plot(data.sub$ar, data.sub$tw, main = "Daily Gold Price Returns vs. Daily Tweet Volatility", 
     xlab = "Return", ylab = "Tweets")


###dataset is split into two groups, data1 & data2, due to a large gap in data.
data1 <- data[data$date < as.Date("2013-08-10"),]
data1$tw <- na.approx(data1$tw) #fill in sporadic missing data
data1$wtw <- na.approx(data1$wtw) #fill in sporadic missing data
data2 <- data[data$date > as.Date("2013-10-03") & data$date < as.Date("2014-07-01"),]
##In data1, the range of returns (r), volatility (ar), and tweet volumes (tw) is larger 
##than in data2
summary(data1[2:4])
summary(data2[2:4])

###In both datasets tweets are autocorrelated
acf(data1$tw)
acf(data2$tw)
##Differencing the tweets so that autocorrelation disappears:
data1$tw.diff <- c(NA,diff(data1$tw, lag = 1, differences = 1))
data2$tw.diff <- c(NA,diff(data2$tw, lag = 1, differences = 1))
acf(na.omit(data1$tw.diff))
acf(na.omit(data2$tw.diff))

###OLS regression of daily price returns as a function of the change in tweet volumes, 
##same day and previous day
modR.data1 <- lm(d~tw.diff + lag(tw.diff), data1)
modR.data2  <- lm(d~tw.diff + lag(tw.diff), data2)
summary(modR.data1)   
summary(modR.data2)

plot(modR.data1)
plot(modR.data2) 

vcovHC(modR.data1)
coeftest(modR.data1, vcov. = vcovHC)

##Including the variable "wtw" (weekend and holiday tweets)                                                                                                                                                   interval):</p>
modRwtw.data1 <- lm(d~tw.diff + lag(tw.diff) + wtw, data1)
modRwtw.data2  <- lm(d~tw.diff + lag(tw.diff) + wtw, data2)
summary(modRwtw.data1)   
summary(modRwtw.data2)

  
###OLS regressions of daily volatility as a function of the change in tweet volumes
modV.data1 <- lm(ar~tw.diff + lag(tw.diff), data1)
modV.data2 <- lm(ar~tw.diff + lag(tw.diff), data2)
summary(modV.data1)
summary(modV.data2)

plot(modV.data1)
plot(modV.data2)  

##Using a robust covariance matrix estimator to test the coefficients of the models
coeftest(modV.data1, vcov. = vcovHC)
coeftest(modV.data2, vcov. = vcovHC)

##adding wtw variable
modVwtw.data1 <- lm(ar~tw.diff + lag(tw.diff) + wtw, data1)
modVwtw.data2  <- lm(ar~tw.diff + lag(tw.diff) + wtw, data2)
summary(modVwtw.data1)   
summary(modVwtw.data2)
