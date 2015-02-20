######################################################################################
#
######################################################################################

##read gold price data
cc <- c("character", "numeric")
p <- read.table("goldPriceData.csv", header = TRUE, sep=",", quote = " ",
                colClasses = cc)
p[,1] <- as.Date(as.character(p[,1]), format = "%m/%d/%Y")

#df "p" has gold price data
p <- p[p$date > as.Date("6/28/2012", format = "%m/%d/%Y", ), ]

#Plot chart of gold price
plot.title = 'Gold Price'
plot.subtitle = ' July 2012-April 2013'
plot <- ggplot(p, aes(x=date, y=price)) + 
  geom_line(colour = "red",size = 1) + 
  ylab("US$/oz") + xlab(" ") + 
  theme(plot.title = element_text(size = 18, colour = "black"))
plot <- plot +  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 
plot

##Gold Price Daily Returns:

#Remove xmas and new year from dataset because its registered as 0% return.  
holidays <- c("2012-12-24","2012-12-25","2012-12-26","2012-12-31","2013-01-01",
              "2013-12-24","2013-12-25","2013-12-26","2013-12-31","2014-01-01")                  
p <- p[! p$date %in% as.Date(holidays), ]

#Calculate daily returns
pdata <- na.omit(mutate(p, d = ((lag(p$price)-p$price)/p$price)*100)) #create daily pct change
#ar= absolute value of daily pct change
pdata$ar <- abs(pdata$d) 

#Plot chart of daily returns
a <- ggplot(data=pdata, aes(x=date, y=d, format(scientific=FALSE)))
theme_new <- theme_set(theme_bw())
a + layer(
  geom = "line",
  stat = "identity", 
  postition="dodge") + ggtitle("Daily % Change in Gold Price") +
  ylab("% Change") + xlab(" ")

#Plot histogram of daily returns
hist(pdata$d, breaks = 100, main = "Distribution of Daily Gold Returns", 
     xlab = "Return", ylab = "Frequency")

