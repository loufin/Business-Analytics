#############################################################
# Problem 7.1 
#############################################################

# three binary variables for categorical 
cols <- c("Profession.stat", "Profession.IT","Profession.other","Years")

data <- list (c(1, 0, 0),
  c(0, 0, 1),
  c(0, 1, 0),
  c(1, 1.1, 1)
  #,c(0, 1, NA)
)

data.df <- as.data.frame(data)
names(data.df) <- cols

# two binary variables for categorical 
#0,0 = other
# 1,0 = stat
# 0,1 = IT
cols1 <- c("Profession.1", "Profession.2","Years")

data1 <- list (c(1, 0, 0),
              c(0, 1, 0),
              c(1, 1.1, 1)
              #,c(0, 1, NA)
)

data1.df <- as.data.frame(data1)
names(data1.df) <- cols1

# b -calculate Euclidean distance 

xdist <- as.matrix(dist(data.df, method="euclidean"))
xdist.df <- as.data.frame(xdist)

xdist1 <- as.matrix(dist(data1.df, method="euclidean"))
xdist1.df <- as.data.frame(xdist1)

# C - use KNN 
data.df$course_taken <- list(0,1,1)
data1.df$course_taken <- list(0,1,1)
library(FNN)

#3 dummies
data.df <- as.data.frame(lapply(data.df, unlist))
nn <- knn(train = data.df[1:2,], test = data.df[3,], 
          cl= data.df[1:2,5], k = 1)


nn
nn[1]

#2 dummies 
data1.df <- as.data.frame(lapply(data1.df, unlist))
nn1 <- knn(train = data1.df[1:2,], test = data1.df[3,], 
          cl= data1.df[1:2,4], k = 1)
nn1
nn1[1]

#############################################################
# Problem 7.2
#############################################################

bank.df <- read.csv("UniversalBank.csv")

#get rid of age and zip code 
bank.df <- bank.df[, -c(1,5)]

set.seed(1)
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1])  
valid.index <- setdiff(row.names(bank.df), train.index)  
train.data <- bank.df[train.index, ]
valid.data <- bank.df[valid.index, ]

# A KNN
library(class)

customer.df <- data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Education=2, Mortgage=0, Securities.Account=0, CD.Account=0, Online=1, CreditCard=1)
#colnames(customer.df) <- colnames(bank.df)

# initialize normalized training, validation data, complete data frames to originals
train.norm.data <- train.data
valid.norm.data <- valid.data

# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.data[, -c(8)], method=c("center", "scale"))
train.norm.data[, -c(8)] <- predict(norm.values, train.data[, -c(8)])
valid.norm.data[, -c(8)] <- predict(norm.values, valid.data[, -c(8)])
customer.norm.df <- predict(norm.values, customer.df)


nn <- knn(train = train.norm.data[, -c(8)], test = customer.norm.df, 
          cl = train.norm.data[, 8], k = 1)

# part B 

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
valid.norm.data$Personal.Loan <- as.factor(valid.norm.data$Personal.Loan)
# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train = train.norm.data[, -c(8)], test = valid.norm.data[-c(8)], 
                  cl = train.norm.data[, 8], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.data[, 8])$overall[1] 
}

accuracy.df

knn5.pred <- knn(train = train.norm.data[, -c(8)], test = valid.norm.data[-c(8)], 
                cl = train.norm.data[, 8], k = 5)
confusionMatrix(knn5.pred, valid.norm.data$Personal.Loan)

# D - predict customer 
customer2.df <- data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Education=2, Mortgage=0, Securities.Account=0, CD.Account=0, Online=1, CreditCard=1)
customer2.norm.df <- predict(norm.values, customer.df)
knn.new <- knn(train = train.norm.data[, -c(8)], test = customer.norm.df, 
          cl = train.norm.data[, 8], k = 5)
knn.new

# E - split into  train, validation, test 

set.seed(1)
train.rows <- sample(rownames(bank.df), dim(bank.df)[1]*0.5)
#set the validation rows to be 50% of the dataset, excluding the training rows 
valid.rows <- sample(setdiff(rownames(bank.df), train.rows), 
                     dim(bank.df)[1]*0.3)
# set the test rows to be be the remaining rows, excluding training and validation rows 

test.rows <- setdiff(rownames(bank.df), union(train.rows, valid.rows))

train.data <- bank.df[train.rows, ]
valid.data <- bank.df[valid.rows, ]
test.data <- bank.df[test.rows, ]

library(class)
train.norm.data <- train.data
valid.norm.data <- valid.data
test.norm.data <- test.data

# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.data[, -c(8)], method=c("center", "scale"))
train.norm.data[, -c(8)] <- predict(norm.values, train.data[, -c(8)])
valid.norm.data[, -c(8)] <- predict(norm.values, valid.data[, -c(8)])
test.norm.data[, -c(8)] <- predict(norm.values, test.data[, -c(8)])

accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
valid.norm.data$Personal.Loan <- as.factor(valid.norm.data$Personal.Loan)
# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train = train.norm.data[, -c(8)], test = valid.norm.data[-c(8)], 
                  cl = train.norm.data[, 8], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.data[, 8])$overall[1] 
}

accuracy.df

# chosen k = 3
knn3.pred <- knn(train = train.norm.data[, -c(8)], test = valid.norm.data[-c(8)], 
                 cl = train.norm.data[, 8], k = 3)
confusionMatrix(knn3.pred, valid.norm.data$Personal.Loan)

test.norm.data$Personal.Loan <- as.factor(test.norm.data$Personal.Loan)
knn3.pred <- knn(train = train.norm.data[, -c(8)], test = test.norm.data[-c(8)], 
                 cl = train.norm.data[, 8], k = 3)
confusionMatrix(knn3.pred, test.norm.data$Personal.Loan)

#############################################################
# Problem 7.3 
#############################################################
housing.df <- read.csv("BostonHousing.csv")

housing.df <- housing.df[,-c(14)]

set.seed(1)
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])  
valid.index <- setdiff(row.names(housing.df), train.index)  
train.data <- housing.df[train.index, ]
valid.data <- housing.df[valid.index, ]

# A KNN
library(class)

# initialize normalized training, validation data, complete data frames to originals
train.norm.data <- train.data
valid.norm.data <- valid.data

# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.data[, -c(13)], method=c("center", "scale"))
train.norm.data[, -c(13)] <- predict(norm.values, train.data[, -c(13)])
valid.norm.data[, -c(13)] <- predict(norm.values, valid.data[, -c(13)])

accuracy.df <- data.frame(k = seq(1, 5, 1), rmse = rep(0, 5), mae = rep(0,5))

# compute knn for different k on validation.
library(forecast)
for(i in 1:5) {
  knn.pred <- class::knn(train = train.norm.data[, -c(13)], test = valid.norm.data[-c(13)], 
                  cl = train.norm.data[, 13], k = i)
  knn.pred <- as.numeric(knn.pred)
  accuracy.df[i, 2] <- RMSE(valid.norm.data$MEDV, knn.pred)
  accuracy.df[i, 3] <- MAE(valid.norm.data$MEDV, knn.pred)
}

# part B 

house.df <- data.frame(list(0.2, 0, 7, 0 ,538, 6, 62,4.7, 4, 307, 21, 10))
colnames(house.df) <- colnames(housing.df[,-c(13)])

house.norm.df <- house.df
house.norm.df <- predict(norm.values, house.norm.df)

knn2.pred <- class::knn(train = train.norm.data[, -c(13)], test = house.norm.df, 
                       cl = train.norm.data[, 13], k = 2)

knn2.pred <- as.numeric(knn2.pred)
knn2.pred

# part c 
knn2.pred <- class::knn(train = train.norm.data[, -c(13)], test = train.norm.data[,-c(13)], 
                        cl = train.norm.data[, 13], k = 2)
knn2.pred <- as.numeric(knn2.pred)
RMSE(train.norm.data$MEDV, knn2.pred)
MAE(train.norm.data$MEDV, knn2.pred)
