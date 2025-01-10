## The library "arules" implements association rules 
install.packages("arules")
library ("arules") 
data("Epub") #Epub is a transactional dataset online journals 
Epub 

summary(Epub) 

## Number of transaction per year 
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y") 
table(year)
Epub2003 <- Epub[year == "2003"] #only want transactions from 2003
length(Epub2003) 
image(Epub2003)

## Look at the first 5 transactions 
inspect(Epub2003[1:5]) 

## See how many times each item was checked out 
itemFrequencyPlot(Epub)
#only plot items that have a support greater than .01 or 1%; 0.8 is change the name to a smaller font
itemFrequencyPlot(Epub, support = 0.01, cex.names = 0.8) 

## The function for generating association rules 
## is apriori (which is the name of a particular algorithm) 
## Generates rules that have a single item as their consequent 
rules <- apriori(Epub) 
rules  ## No(0) rules generated 
## By default, apriori finds rules with a support of at least 10% and confidence of at least 80% 
## Over ride the default minimum support and confidence 
rules <- apriori(Epub, parameter = list(support = 0.001,confidence = 0.2)) 
rules 
summary(rules) 
inspect(rules) 
## Use inspect to see the rules 
inspect(head(sort(rules, by = "confidence"), n = 3)) 
