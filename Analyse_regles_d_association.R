setwd("~/GitHub/Projet_IF28")
library(arules)

# oldpeak: double
# age, trestbps, chol and thalach: int

#txn = read.transactions(file="~/GitHub/Projet_IF28/Data/heart.csv", rm.duplicates=FALSE, format="basket", sep=",", cols=0)

###read csv
heart = read.csv("~/GitHub/Projet_IF28/Data/heart.csv")

###add id column
heart$id <- rownames(heart)
###delete oldpeak (double)
col_order <- c("id", "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", 
               "exang", "slope", "ca", "thal", "target")
heart <- heart[, col_order] #order columns

###change values
#maximum heart rate
heart$thalach[heart$thalach <= 130] <- "low"
heart$thalach[heart$thalach > 130 & heart$thalach != "low"] <- "high"
#cholestoral in mg/dl 
heart$chol[heart$chol <= 189] <- "good"
heart$chol[heart$chol > 189 & heart$chol != "good"] <- "bad"
#resting blood pressure (in mm Hg)
heart$trestbps[heart$trestbps > 120] <- "bad"
heart$trestbps[heart$trestbps <= 120 & heart$trestbps != "bad"] <- "good"
#age
heart$age[heart$age <= 60] <- "young"
heart$age[heart$age > 60 & heart$age!= "young"] <- "old"

### inspect csv
str(heart)
head(heart)

### write processed csv
write.csv(heart[2:14],"~/GitHub/Projet_IF28/Data/heart_tr.csv", quote = FALSE, row.names = TRUE)


### read transaction
txn = read.transactions(file="~/GitHub/Projet_IF28/Data/heart_tr.csv", rm.duplicates=TRUE, 
                        format="basket", sep=",", cols=1)
### inspect transaction
#inspect(head(txn))
#summary(txn)
#dim(txn)

### item frequency
itemFrequencyPlot(txn, support = 0.1, topN=10, type="absolute")

### extract rules
rules <- apriori(txn, parameter = list(minlen=2, sup = 0.4, conf = 0.6, target="rules"))
summary(rules)
inspect(rules)

#interestingRules <- subset(rules, subset=rhs%in%"target=1" & lift>1)

