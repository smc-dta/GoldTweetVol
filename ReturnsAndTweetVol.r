######################################################################################
#
######################################################################################

#Dividing daily tweet data into sets "tw" and "wtw".  
#The "tw" tweets correspond to tweets sent on trading days when the markets are open.
#The "wtw" tweets are those sent on weekends and holidays.  
tw <- gt[gt$date %in% p$date, ]
wtw <- gt[!(gt$date %in% p$date), ]
#days = number of days between current date and date of next row
for(i in 1:nrow(wtw)){wtw$days[i] <- (wtw[i+1,1]-wtw[i,1])}
wtw$days <- as.numeric(wtw$days)
#wtw[! wtw$days == 1 & ! wtw$days == 6, ] #will show exceptions

wtw[nrow(wtw),3] <- 1 #Eliminates NA in last row
#correct for holidays
for(i in 1:nrow(wtw)){
  if (wtw$date[i] == as.Date("2012-12-26") | wtw$date[i] == as.Date("2013-12-29")){
    wtw$days[i] <- 1
  }
}

wtw$DateNew <- lapply(wtw$days, func2)




func1 <- function(){
  for(i in 1:nrow(wtw)){
    if(wtw$days[i] > 1){
      wtw$DateNew[i] <- wtw$date[i] + 1
    } else {
      wtw$DateNew[i] <- wtw$date[i] + 2
    }
  }
}

func2 <- function(days){
  if(wtw$days[i] > 1){
    wtw$DateNew[i] <- wtw$date[i] + 1
  } else {
    wtw$DateNew[i] <- wtw$date[i] + 2
  }
}




wtw %>% select(days) %>  
  
iris %>% select(Sepal.Length:Petal.Width) %>%
  mutate(sumVar = rowSums(.)) %>% left_join(iris)




wtw[ ,4] <- as.Date(wtw$DateNew, origin = "1970-01-01")
wtw[ ,1] <- wtw$DateNew
wtw <- wtw[,1:2]
wtw <- ddply(wtw, "Date", summarize, n = sum(n))
gt <- merge(tw,wtw, all = TRUE, by ='Date') # merge tweets and weekend tweets into gt
names(gt)<- c("d", "tw", "wtw")
gt[is.na(gt)]<- 0

summary(gt$tw)
