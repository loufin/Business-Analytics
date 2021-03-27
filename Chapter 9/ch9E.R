# 9.1

auctions.df <- read.csv("eBayAuctions.csv", header=T)

train.index <- sample(rownames(auctions.df), 0.6*dim(auctions.df)[1])
valid.index <- setdiff(rownames(auctions.df), train.index)

train.df <- auctions.df[train.index,]
valid.df <- auctions.df[valid.index,]

library(rpart)
library(rpart.plot)

class.tree <- rpart(Competitive. ~ ., data = auctions.df, 
                    control = rpart.control(maxdepth = 7, minbucket = 50), method = "class")

prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 


class.tree <- rpart(Competitive. ~ ClosePrice+OpenPrice+sellerRating+Duration+endDay, data = auctions.df, 
                    control = rpart.control(maxdepth = 7, minbucket = 50), method = "class")
pruned.ct <- prune(class.tree, 
                   cp = class.tree$cptable[which.min(class.tree$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  


#9.2
flights.df <- read.csv("FlightDelays.csv", header=T)
flights.df$DAY_WEEK <- as.factor(flights.df$DAY_WEEK)
flights.df$DEP_TIME <- cut(flights.df$DEP_TIME, breaks = seq(0,2400,300), labels = c("3","6","9","12","15","18","21","24"), include.lowest = T, right = T)

flights.df <- flights.df[,-11]
flights.df$Weather <- as.factor(flights.df$Weather)
flights.df$FL_NUM <- as.factor(flights.df$FL_NUM)
flights.df$DISTANCE <- cut(flights.df$DISTANCE, breaks = seq(165,235,10))

train.index <- sample(rownames(flights.df), 0.6*dim(flights.df)[1])
valid.index <- setdiff(rownames(flights.df), train.index)

train.df <- flights.df[train.index,]
valid.df <- flights.df[valid.index,]

library(rpart)
library(rpart.plot)

class.tree <- rpart(Flight.Status ~ CARRIER+DEP_TIME+DEST+DISTANCE+ORIGIN+Weather+DAY_WEEK, data = flights.df, 
                    control = rpart.control(maxdepth = 8, minbucket = 50), method = "class")
pruned.ct <- prune(class.tree, cp = 0.001)

length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = "auto", split.font = 2, varlen = 3) 

library(caret)
class.tree.point.pred.flights <- predict(class.tree,flights.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(class.tree.point.pred.flights, as.factor(flights.df$Flight.Status))
### repeat the code for the validation set, and the deeper tree

