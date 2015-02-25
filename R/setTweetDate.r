######################################################################################
#Adjusts date of tweets to correspond to market trading days
######################################################################################

tw <- gt[gt$date %in% p$date, ]
wtw <- gt[!(gt$date %in% p$date), ]
for(i in 1:nrow(wtw)){
  wtw$days[i] <- (wtw[i+1,1] - wtw[i,1])
}
wtw$days <- as.numeric(wtw$days)

##correcting holidays
for(i in 1:nrow(wtw)){
  if (wtw$date[i] == as.Date("2012-12-26")| wtw$date[i] == as.Date("2013-12-29")){
   wtw$days[i] <- 1
  }
}

##correcting last observation
wtw[nrow(wtw), 3] <- 6

##wtw$DateNew is the next day the market is open
wtw$DateNew <- ifelse(wtw$days > 1, wtw$DateNew <- wtw$date + 1,
                      wtw$DateNew <- wtw$date + 2)
wtw$date <- as.Date(wtw$DateNew, origin = "1970-01-01")
wtw <- wtw[, -4] #wtw$DateNew variable no longer needed
wtw <- wtw %>% group_by(date)
wtw <- na.omit(wtw %>% summarise(n = sum(n)))

gt <- merge(tw,wtw, all = TRUE, by ='date') 
names(gt) <- c("date", "tw", "wtw")
gt[is.na(gt)]<- 0
