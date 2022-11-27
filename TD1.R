library(arules)
# library(ggplot2)
# library(dplyr)
# library(readr)
# library(maps)
# library(tidyverse)
# library(plotrix)
# library(rgl)
# library(reshape2)
# library(shiny)

txn = read.transactions(file="C:/Users/chino/Documents/GitHub/IF28/transaction.csv", rm.duplicates= FALSE,format = "single",sep=",",cols =c(1,2))
# txn <- read.csv("C:/Users/chino/Documents/GitHub/IF28/transaction.csv")
inspect(txn)
itemFrequencyPlot(txn)
basket_rules <- apriori(txn,parameter = list(sup = 0.5,conf = 0.9,target="rules"))
inspect(basket_rules)
inspect(basket_rules[1])

library(arules)
data("Epub")
Epub
summary(Epub)
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)
Epub2003 <- Epub[year == "2003"]
length(Epub2003)
image(Epub2003)
transactionInfo(Epub2003[size(Epub2003) > 20])
inspect(Epub2003[1:5])
as(Epub2003[1:5], "list")
EpubTidLists <- as(Epub, "tidLists")
EpubTidLists
as(EpubTidLists[1:3], "list")


data("AdultUCI")
dim(AdultUCI)
AdultUCI[1:2, ]
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]],c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week" ]], c(0, 25, 40, 60, 168)),labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain" ]], c(-Inf, 0, median(AdultUCI[["capital-gain" ]][AdultUCI[["capital-gain" ]] > 0]),Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss" ]], c(-Inf, 0, median(AdultUCI[["capital-loss" ]][AdultUCI[["capital-loss" ]] > 0]),Inf)), labels = c("none", "low", "high"))

Adult <- as(AdultUCI, "transactions")
Adult
summary(Adult)
rules <- apriori(Adult, parameter = list(support = 0.01,confidence = 0.6))
itemFrequencyPlot(Adult, support = 0.1, cex.names = 0.8)

rules
summary(rules)
rulesIncomeSmall <- subset(rules, subset = rhs %in%"income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in%"income=large" & lift > 1.2)

inspect(head(sort(rulesIncomeSmall, by = "confidence"),n = 3))
inspect(head(sort(rulesIncomeLarge, by = "confidence"),n = 3))
write(rulesIncomeSmall, file = "data.csv", sep = ",",col.names = NA)

if(!require("pmml")){install.packages("pmml")}
library("pmml")
rules_pmml <- pmml(rulesIncomeSmall)
saveXML(rules_pmml, file = "data.xml")

library("arules")
library("arulesViz")
data("Groceries")
rules <- apriori(Groceries, parameter = list(supp = 0.001,conf = 0.8))

rules
plot(rules, control=list(jitter=2))

rules_high_lift <- head(sort(rules, by="lift"), 3)
inspect(rules_high_lift)

plot(rules_high_lift, method="graph", control=list(type="items"))




