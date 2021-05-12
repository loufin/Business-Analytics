
library(arules)
courses.df <- read.csv("Coursetopics.csv")
View(courses.df)

#remove rows with all zeros
courses.df <- courses.df[rowSums(courses.df)!=0,]

#courses <- courses[apply(courses, 1, function(x) !all(x==0)),]

courses.mat <- as.matrix(courses.df)

courses.trans <- as(courses.mat, "transactions")
inspect(courses.trans)
courses.trans
itemFrequencyPlot(courses.trans)

rules <- apriori(courses.trans, parameter = list(supp = 0.01, conf = 0.1, target = "rules"))

inspect(head(sort(rules, by = "lift"), n = 34))
rules.tbl <- inspect(sort(rules,by="conf"))
rules.tbl[rules.tbl$support >= 0.04 & rules.tbl$confidence >= 0.1,]

