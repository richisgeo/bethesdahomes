library(ggplot2) 
library(doBy)
library(lubridate)
library(data.table)
options(scipen=999)  
#read   
b <- read.csv("bethesda.csv")
b$LAST.SALE.DATE <- as.Date(b$LAST.SALE.DATE)   
b$CITY <- toupper(b$CITY)
b$YEAR <- year(b$LAST.SALE.DATE)
b$MONTH <- as.factor(month(b$LAST.SALE.DATE , label = TRUE))
s <- summaryBy(LAST.SALE.PRICE  ~ CITY + YEAR , data = b, FUN = mean)

s <- renameCol(s,3,"MEAN")
s$YEAR <- as.factor(s$YEAR)
s$CITY <- as.factor(s$CITY)


p = ggplot(s, aes(x=YEAR, y = LAST.SALE.PRICE.mean, fill=CITY)) + geom_bar(stat = "identity" , position = "dodge") 
p + facet_wrap(~ CITY)





s2 <- table(format(b$LAST.SALE.DATE,"%b-%Y"))
s2
barplot(s2)

unique(b$STATUS)
b$MonYr <- format(b$LAST.SALE.DATE,"%b-%Y")
sagg <- aggregate (  STATUS ~ MonYr + CITY , data = b , FUN = length)
sagg
barplot(sagg)
sagg
 sagg[sagg$MonYr == "Apr-2013",]

p = ggplot(sagg, aes(x=as.factor(MonYr), y = STATUS, colour=CITY)) + geom_line(aes(group = CITY)) 
p
p + facet_wrap(~ CITY)