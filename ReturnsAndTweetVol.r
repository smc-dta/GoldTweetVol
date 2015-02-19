######################################################################################
#
######################################################################################

#Dividing daily tweet data into sets "tw" and "wtw".  
#The "tw" tweets correspond to tweets sent on trading days when the markets are open.
#The "wtw" tweets are those sent on weekends and holidays.  
tw <- gt[gt$date %in% p$date, ]
wtw <- gt[!(gt$date %in% p$date), ]
for(i in 1:nrow(wtw)){wtw$days[i] <- (wtw[i+1,1]-wtw[i,1])}
wtw$days <- as.numeric(wtw$days)

#Run this to see exceptions
#wtw[! (wtw$date.1 == 1 | wtw$date.1 == 6), ]
2012-12-26
2013-12-29 
wtw1 add 1
wtw2 add 3
wtw3 add 1
wtw4 add 1
wtw5 add 1
wtw6 add 1
wtw7 add 2
wtw  add 1

#wtw$NewDate <- wtw$date #do this to set up class
#wtw[nrow(wtw), 3] <- 1  am not sure why this is here

func1 <- function(days){
  for(i in 1:nrow(wtw)){
    if(wtw$days[i] > 1 == TRUE){wtw$DateNew[i] <- wtw$date[i] + 1
    } else if (wtw$date[i] == as.Date("2012-12-26")| wtw$date[i] == as.Date("2013-12-29")){
      wtw$DateNew[i] <- wtw[i,1] + 1
    } else {wtw$DateNew[i] <- wtw$date[i] + 2}
  }
}

fun1 <- function(days){
  for(i in 1:nrow(wtw)){wtw$DateNew[i] <- days[i]}
}

mutate(wtw, func1(days))
  
wtw %>% class(days)  
  
  


wtw[ ,4] <- as.Date(wtw$DateNew, origin = "1970-01-01")
wtw[ ,1] <- wtw$DateNew
wtw <- wtw[,1:2]
wtw <- ddply(wtw, "Date", summarize, n = sum(n))
gt <- merge(tw,wtw, all = TRUE, by ='Date') # merge tweets and weekend tweets into gt
names(gt)<- c("d", "tw", "wtw")
gt[is.na(gt)]<- 0

summary(gt$tw)
