library(neuralnet)
toyota.df <- read.csv("ToyotaCorolla.csv")
View(toyota.df)

dummyFT <- fastDummies::dummy_cols(toyota.df$Fuel_Type)
toyota.df$CNG <- dummyFT$.data_CNG
toyota.df$Diesel <- dummyFT$.data_Diesel
toyota.df$Petrol <- dummyFT$.data_Petrol

str(toyota.df)
toyota.df <- toyota.df[,c(4,7,8,9,12,14,17,19,21,25,26,28,30,34,39,40,41,42,3)]

str(toyota.df)
toyota.df <- toyota.df[,-3]
str(toyota.df)

for(i in c(1:18))
{
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
nn <- neuralnet(Price ~ ., data = trainData, linear.output = T, hidden = c(5,3))
# linear.output=T for numerical predictions, F for categorical
# hidden = 5 for one layer with 5 nodes; 
# hidden = c(5,5) for two layers with 5 nodes


# plot network
plot(nn, rep="best")

View(trainData)
library(caret)
training.prediction <- compute(nn, trainData[,-18])
trainData$predicted <- training.prediction$net.result
trainData$error <- abs(trainData$Price-trainData$predicted)
#RMSE
RMSE(trainData$Price,trainData$predicted)

# % error
mean(trainData$error)*100

validation.prediction <- compute(nn, validData[,-18])
validData$predicted <- validation.prediction$net.result
validData$error <- abs(validData$Price-validData$predicted)
#RMSE
RMSE(validData$Price,validData$predicted)

# % error
mean(validData$error)*100

View(validData)
