bike.df <- read.csv("bikedata.csv")

#bike.num.df <- bike.df[,sapply(bike.df, is.numeric)]

# just the count 
bike.num.df <- bike.df[,-c(1,2,10,14,15)]

# just the count of casual
#bike.num.df <- bike.df[,-c(1,2,15,16)]

# just count of registered
#bike.num.df <- bike.df[,-c(1,2,14,16)]

#now convert int vars to numeric
bike.num.df[] <- lapply(bike.num.df, function(x) {
  if(is.integer(x)) x<-as.numeric(x) else x
  
})
library(gplots)
# heatmap with values 
heatmap.2(cor(bike.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(bike.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


library(forecast)

set.seed(1)
train.rows <- sample(seq_len(nrow(bike.num.df)), size = floor(0.9 * nrow(bike.num.df)))

train.data <- bike.num.df[train.rows, ]
test.data <- bike.num.df[-train.rows, ]

bike.lm <- lm(cnt~., data = train.data)
pred.train <- predict(bike.lm)

pred.validation <- predict(bike.lm, newdata=test.data)

## evaluate performance
# training
accuracy(pred.train, bike.num.df[train.rows,]$cnt)
# validation
accuracy(pred.validation, bike.num.df[-train.rows,]$cnt)


library(jtools)
summ(bike.lm)

library(forecast)
all.residuals <- test.data$cnt - pred.validation
# plot residuals 
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

# do BACKWARD stepwise function 
bike.lm.step <- step(bike.lm, direction = "backward")
summary(bike.lm.step)  # Which variables were dropped?
bike.lm.step.pred <- predict(bike.lm.step, test.data)
accuracy(bike.lm.step.pred, test.data$cnt)

# FORWARD STEPWISE 
# create model with no predictors
bike.lm.null <- lm(cnt~1, data = train.data)
# use step() to run forward regression.
bike.lm.step <- step(bike.lm.null, scope=list(lower=bike.lm.null, upper=bike.lm), direction = "forward")
summary(bike.lm.step)  # Which variables were added?
bike.lm.step.pred <- predict(bike.lm.step, test.data)
accuracy(bike.lm.step.pred, test.data$cnt)