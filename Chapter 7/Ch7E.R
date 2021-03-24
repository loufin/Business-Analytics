############   7.1   #####################
data1.df <- as.data.frame(list(c(1,0),c(0,0),c(0,1),c(1,1.1),c(0,1)))
names(data1.df) <- c("stat","IT","other","year","tookcourse")

new.df <- as.data.frame(list(0,1,0,1,NA))
new.norm1.df <- new.df
#new.norm.values <- preProcess(new.df[, 1:4], method=c("center"))
#new.norm1.df[, 1:4] <- predict(new.norm.values, new.df[, 1:4])
names(new.norm1.df) <- c("stat","IT","other","year","tookcourse")

norm1.df <- data1.df

######Euclidean distance#######
library(abind)
combined.df <- rbind(norm1.df, new.norm1.df)
combined.df <- combined.df[,-5]
row.names(combined.df) <- c("Cust1", "Cust2", "Pros1")
#View(combined.df)
#create distance distribution
xdist <- dist(combined.df, method="euclidean")
#coerve into matrix format
xdistmat <- as.matrix(xdist)
#coerce into data fram
xdist.df <- as.data.frame(xdistmat)
#set row and column names 
names(xdist.df) <- row.names(combined.df)
row.names(xdist.df) <- row.names(combined.df)

#View distance matrix
#View(xdist.df)
######################################

library(caret)
#norm.values <- preProcess(data1.df[, 1:4], method=c("center"))
#norm1.df[, 1:4] <- predict(norm.values, data1.df[, 1:4])

library(FNN)
#2 dummies
nn <- knn(train = norm1.df[, c(1,2,4)], test = new.norm1.df[,c(1,2,4)], 
          cl = norm1.df[, 5], k = 1)
nn
nn[1]

#3 dummies
nn <- knn(train = norm1.df[, 1:4], test = new.norm1.df[,1:4], 
          cl= norm1.df[, 5], k = 1)
nn
nn[1]

############   7.2   #####################
uni.df <- read.csv("UniversalBank.csv")
set.seed(111)
train.index <- sample(row.names(uni.df), 0.6*dim(uni.df)[1])  
valid.index <- setdiff(row.names(uni.df), train.index)  
train.df <- uni.df[train.index, ]
valid.df <- uni.df[valid.index, ]
## new customer
new.df <- data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Education=2, Mortgage=0, Securities.Account=0, CD.Account=0, Online=1, CreditCard=1)

#plot(uni.df)

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
uni.norm.df <- uni.df
# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.df[, -c(1,5,10)], method=c("center", "scale"))
train.norm.df[, -c(1,5,10)] <- predict(norm.values, train.df[, -c(1,5,10)])
valid.norm.df[, -c(1,5,10)] <- predict(norm.values, valid.df[, -c(1,5,10)])
uni.norm.df[, -c(1,5,10)] <- predict(norm.values, uni.df[, -c(1,5,10)])
new.norm.df <- predict(norm.values, new.df)

# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).
library(class)
nn <- knn(train = train.norm.df[, -c(1,5,10)], test = new.norm.df, 
          cl = train.norm.df[, 10], k = 1)

row.names(train.df)[attr(nn, "nn.index")]




### Table 7.3

library(caret)

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, -c(1,5,10)], valid.norm.df[, -c(1,5,10)], 
                  cl = train.norm.df[, 10], k = i)
  accuracy.df[i, 2] <- confusionMatrix(as.factor(knn.pred), as.factor(valid.norm.df[, 10]))$overall[1] 
}
accuracy.df



#### Table 7.4

knn.pred.new <- knn(uni.norm.df[, -c(1,5,10)], new.norm.df, 
                    cl = uni.norm.df[, 10], k = 4)
row.names(train.df)[attr(nn, "nn.index")]


