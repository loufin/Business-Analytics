####################################
# Problem 9.1
####################################
auctions.df <- read.csv("eBayAuctions.csv", header=T)
auctions.df$Category <- as.factor(auctions.df$Category)
auctions.df$Competitive. <- as.factor(auctions.df$Competitive.)

library(plyr)
auctions.df$Competitive. <- revalue(auctions.df$Competitive., c("1" = "Comp", "0" = "Non-Comp"))

set.seed(54)
train.index <- sample(rownames(auctions.df), 0.6*dim(auctions.df)[1])
valid.index <- setdiff(rownames(auctions.df), train.index)

train.df <- auctions.df[train.index,]
valid.df <- auctions.df[valid.index,]

library(rpart)
library(rpart.plot)

# a - fit classification tree to competitive with all variables
class.tree <- rpart(Competitive. ~ ., data = auctions.df, 
                    control = rpart.control(maxdepth = 7, minbucket = 50), method = "class")

prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 


class.tree <- rpart(Competitive. ~ ClosePrice+OpenPrice+sellerRating+Duration+endDay, data = auctions.df, 
                    control = rpart.control(maxdepth = 7, minbucket = 50), method = "class")
pruned.ct <- prune(class.tree, 
                   cp = class.tree$cptable[which.min(class.tree$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

# D - only using vars known before auction
class.tree <- rpart(Competitive. ~ OpenPrice+sellerRating+Category, data = auctions.df, 
                    control = rpart.control(maxdepth = 7, minbucket = 50), method = "class")

prp(class.tree, type = 1, extra = "auto", split.font = 2, varlen = -10) 

# E - plot seller rating and opening price, color by competetive 

library(ggplot2)

ggplot(train.df, aes(y = OpenPrice, x = sellerRating, colour = Competitive.)) +
  geom_point(alpha = 1) +
  geom_hline(yintercept = 3.6, color = "coral4") +
  geom_hline(yintercept = 10, color = "coral4") +
  geom_hline(yintercept = 1.8, color = "coral4") +
  geom_vline(xintercept = 562, color = "coral4") +
  ylim(0,25) +
  xlim(0,6000)

# f confusion matrix and lift chart 
pred.class <- predict(class.tree, valid.df, type = "class")

library(caret)
valid.df$Competitive. <- as.factor(valid.df$Competitive.)
confusionMatrix(pred.class, valid.df$Competitive.,)



####################################
# Problem 9.2 
####################################

flights.df <- read.csv("FlightDelays.csv", header=T)

#change to categorical 15 minute chunks 
flights.df$DAY_WEEK <- as.factor(flights.df$DAY_WEEK)

# binning the departure time into eight bins, so every 3 hours 
flights.df$DEP_TIME <- cut(flights.df$DEP_TIME, breaks = seq(0,2400,300), labels = c("300","600","900","1200","1500","1800","2100","2400"), include.lowest = T, right = T)

# remove day of month 
flights.df <- flights.df[,-11]

#make weather and flight number categorical  
flights.df$Weather <- as.factor(flights.df$Weather)
flights.df$FL_NUM <- as.factor(flights.df$FL_NUM)

# bin distance into 8 groups by 10 
flights.df$DISTANCE <- as.numeric(flights.df$DISTANCE)
flights.df$DISTANCE <- cut(flights.df$DISTANCE, breaks = seq(165,235,10))
flights.df$DISTANCE <- as.factor(flights.df$DISTANCE)
#split into train and test 
set.seed(625)
train.index <- sample(rownames(flights.df), 0.6*dim(flights.df)[1])
valid.index <- setdiff(rownames(flights.df), train.index)

train.df <- flights.df[train.index,]
valid.df <- flights.df[valid.index,]

library(rpart)
library(rpart.plot)

#a - fit classification tree with everything except DEP_TIME
class.tree <- rpart(Flight.Status ~ CRS_DEP_TIME+CARRIER+DEST+DISTANCE+ORIGIN+Weather+DAY_WEEK, data = train.df, 
                    control = rpart.control(maxdepth = 8, minbucket = 50), method = "class")
prp(class.tree, type = 1, extra = "auto", split.font = 2, varlen = -10)
#prune tree 
pruned.ct <- prune(class.tree, cp = 0.001)

# how many layers? 
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
#display tree 
prp(pruned.ct, type = 1, extra = "auto", split.font = 2, varlen = -10) 

library(caret)
class.tree.point.pred.flights <- predict(class.tree,flights.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(class.tree.point.pred.flights, as.factor(flights.df$Flight.Status))
### repeat the code for the validation set, and the deeper tree

# B - in writing 

# c - fit tree again, but without Weather
class.tree <- rpart(Flight.Status ~ CRS_DEP_TIME+CARRIER+DEST+DISTANCE+ORIGIN+DAY_WEEK, data = train.df, 
                    control = rpart.control(maxdepth = 8, minbucket = 50), method = "class")

#prune tree 
pruned.ct <- prune(class.tree, cp = 0.001)

prp(class.tree, type = 1, extra = "auto", split.font = 1, varlen = -10) 
prp(pruned.ct, type = 1, extra = "auto", split.font = 1, varlen = -10) 
