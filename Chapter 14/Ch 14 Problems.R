###################################################
# Problem 14.2 
###################################################

library(arules)
courses.df <- read.csv("Coursetopics.csv")

#remove rows with all zeros
courses.df <- courses.df[rowSums(courses.df)!=0,]

courses.mat <- as.matrix(courses.df)
courses.trans <- as(courses.mat, "transactions")
inspect(courses.trans)

# plot relative frequency of each item being true 
itemFrequencyPlot(courses.trans)

#association rules 
rules <- apriori(courses.trans, parameter = list(supp = 0.01, conf = 0.1, target = "rules"))
inspect(head(sort(rules, by = "lift"), n = 34))

rules.tbl <- inspect(sort(rules,by="conf"))
rules.tbl[rules.tbl$support >= 0.04 & rules.tbl$confidence >= 0.1,]

###################################################
# Problem 14.3
###################################################
courses.df <- read.csv("Coursetopics.csv")

#remove rows with all zeros
courses.df <- courses.df[rowSums(courses.df)!=0,]

courses.mat <- as.matrix(courses.df)

library(recommenderlab)
## convert into a realRatingMatrix
r <- as(courses.mat, "binaryRatingMatrix")

# user-based collaborative filtering
UB.Rec <- Recommender(r, "UBCF")
pred <- predict(UB.Rec, r, type="ratings")
as(pred, "matrix")
