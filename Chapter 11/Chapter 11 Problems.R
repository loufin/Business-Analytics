######################################################
# Problem 11.3 
########################################################
library(neuralnet)
toyota.df <- read.csv("ToyotaCorolla.csv")

dummyFT <- fastDummies::dummy_cols(toyota.df$Fuel_Type)
toyota.df$CNG <- dummyFT$.data_CNG
toyota.df$Diesel <- dummyFT$.data_Diesel
toyota.df$Petrol <- dummyFT$.data_Petrol

# only need to use age, KM, fuel, HP, automatic, doors, quarterly tax, mfr guarantee, period
# airco, automatic, CD, windows, sport, tow bar 
toyota.df <- toyota.df[,c(4,7,9,12,14,17,19,21,25,26,28,30,34,39,40,41,42,3)]
str(toyota.df)

# preprocess option 

# manually scale the data to be between [0,1]
for(i in c(1:length(colnames(toyota.df)))){
  a <- min(toyota.df[,i])
  b <- max(toyota.df[,i])
  toyota.df[,i] <- (toyota.df[,i]-a)/(b-a)
}

View(toyota.df)

set.seed(2)
training=sample(row.names(toyota.df), dim(toyota.df)[1]*0.6)
validation=setdiff(row.names(toyota.df), training)

trainData <- toyota.df[training,]
validData <- toyota.df[validation,]

set.seed(1)

# one hidden layer with 2 nodes 
nn2 <- neuralnet(Price ~ ., data = trainData, linear.output = T, hidden = 2)
# linear.output=T for numerical predictions, F for categorical

# one hidden layer with 5 nodes 
nn5 <- neuralnet(Price ~ ., data = trainData, linear.output = T, hidden = 5)

# 2 hidden layers with 5 nodes each 
nn55 <- neuralnet(Price ~ ., data = trainData, linear.output = T, hidden = c(5,5))

# plot network
plot(nn55, rep="best")

library(caret)
training.prediction2 <- compute(nn2, trainData[,-18])
trainData$predicted2 <- training.prediction2$net.result
trainData$error2 <- abs(trainData$Price-trainData$predicted2)

training.prediction5 <- compute(nn5, trainData[,-18])
trainData$predicted5 <- training.prediction5$net.result
trainData$error5 <- abs(trainData$Price-trainData$predicted5)

training.prediction55 <- compute(nn55, trainData[,-18])
trainData$predicted55 <- training.prediction55$net.result
trainData$error55 <- abs(trainData$Price-trainData$predicted55)

#RMSE
RMSE(trainData$Price,trainData$predicted2)
RMSE(trainData$Price,trainData$predicted5)
RMSE(trainData$Price,trainData$predicted55)

# % error
mean(trainData$error2)*100
mean(trainData$error5)*100 
mean(trainData$error55)*100

# now 
validation.prediction2 <- compute(nn2, validData[,-18])
validData$predicted2 <- validation.prediction2$net.result
validData$error2 <- abs(validData$Price-validData$predicted2)

validation.prediction5 <- compute(nn5, validData[,-18])
validData$predicted5 <- validation.prediction5$net.result
validData$error5 <- abs(validData$Price-validData$predicted5)

validation.prediction55 <- compute(nn55, validData[,-18])
validData$predicted55 <- validation.prediction55$net.result
validData$error55 <- abs(validData$Price-validData$predicted55)

#RMSE
RMSE(validData$Price,validData$predicted2)
RMSE(validData$Price,validData$predicted5)
RMSE(validData$Price,validData$predicted55)

# % error
mean(validData$error2)*100
mean(validData$error5)*100
mean(validData$error55)*100

View(validData)
