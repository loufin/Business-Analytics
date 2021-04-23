###########################################
# Problem 12.1 
##########################################

# A - summary statistics 
bank.df <- read.csv("UniversalBank.csv")
bank.org.df <- bank.df
#View(head(bank.df))
str(bank.df)
summary(bank.df)

bank.df <- bank.df[,-5]
bank.df$Personal.Loan <- as.factor(bank.df$Personal.Loan)
bank.num.df <- bank.df[,c(2:6,8,9)]

# convert dummy variables 
dummyFT <- fastDummies::dummy_cols(bank.df$Education)
bank.df$edu_1 <- dummyFT$.data_1
bank.df$edu_2 <- dummyFT$.data_2
bank.df$edu_3 <- dummyFT$.data_3

library(psych)
describeBy(bank.df, bank.df$Personal.Loan)

# B examine model performance 

bank.df <- bank.org.df
bank.df$Personal.Loan <- as.factor(bank.df$Personal.Loan)
bank.df <- bank.df[,-5]
bank.df <- bank.df[,c(1:8,10:13,9)]

train.index <- sample(bank.df$ID, 0.6*dim(bank.df)[1])
valid.index <- setdiff(bank.df$ID, train.index)

train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]

library(MASS)
lda.reg <- lda(train.df[,1:12], train.df[,13])
summary(lda.reg)

pred.train.df <- predict(lda.reg, train.df[,-13], type="raw")

df <- data.frame(actual = train.df$Personal.Loan, predicted = pred.train.df)

df$predicted.class <- as.character(df$predicted.class)
df$predicted.class <- as.numeric(df$predicted.class)

# accuracy
library(forecast)
accuracy(as.numeric(df$actual), as.numeric(df$predicted.class))

# validation
pred.valid.df <- predict(lda.reg, newdata=valid.df[,-13], type="raw")
df <- data.frame(actual = valid.df$Personal.Loan, predicted = pred.valid.df)
df$predicted.class <- as.character(df$predicted.class)
df$predicted.class <- as.numeric(df$predicted.class)
accuracy(as.numeric(df$actual), as.numeric(df$predicted.class))

library(caret)
confusionMatrix(df$actual, as.factor(df$predicted.class))
df$predicted.class <- as.factor(df$predicted.class)

# C - lift chart 
df.lift <- df[order(df$predicted.posterior.0),]
df.lift$cumden <- cumsum(as.numeric(df.lift$actual))/sum(as.numeric(df.lift$actual))
df.lift$perpop <- (seq(nrow(df.lift))/nrow(df.lift))*100
plot(df.lift$perpop,df.lift$cumden,type="l",xlab="% of Population",ylab="% of Loan acceptance")

# decile wise lift chart 
library(gains)

gain <- gains(as.numeric(df.lift$actual), as.numeric(df.lift$predicted.posterior.0))
barplot(gain$mean.resp / mean(as.numeric(df.lift$actual)), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")

# D - logistic regression 
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)
pred <- predict(logit.reg, valid.df, type = "response")

library(caret)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid.df$Personal.Loan))  

df.log.lift <- data.frame(pred, valid.df$Personal.Loan)
df.log.lift <- df.log.lift[order(df.log.lift$pred, decreasing = FALSE),]
df.log.lift$cumden <- cumsum(as.numeric(df.log.lift$valid.df.Personal.Loan))/sum(as.numeric(df.log.lift$valid.df.Personal.Loan))
df.log.lift$perpop <- (seq(nrow(df.log.lift))/nrow(df.log.lift))*100
plot(df.log.lift$perpop,df.log.lift$cumden,type="l",xlab="% of Population",ylab="% of Loan acceptance")

gain.log <- gains(as.numeric(df.log.lift$valid.df.Personal.Loan), as.numeric(df.log.lift$pred))
barplot(gain.log$mean.resp / mean(as.numeric(df.log.lift$valid.df.Personal.Loan)), names.arg = gain.log$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")

# E - 1000 more customers, costs $1, gains 50
# take the bottom 1000 customers 
avg_value = 49 * df.lift$predicted.posterior.1
sum(head(avg_value, 1000))

# F - change value of the cutoff and see how it effects misclassification
df$low_cutoff <- as.factor(ifelse(df$predicted.posterior.1 > 0.4, 1, 0))
df$high_cutoff <- as.factor(ifelse(df$predicted.posterior.1 > 0.6, 1, 0))
confusionMatrix(df$actual, as.factor(df$low_cutoff))
confusionMatrix(df$actual, as.factor(df$high_cutoff))

###########################################
# Problem 12.2 
##########################################
sys.df <- read.csv("SystemAdministrators.csv")

# scatterplot 
library(ggplot2)

ggplot(sys.df, aes(y = Training, x = Experience, colour = Completed.task)) +
  geom_point(alpha = 1) +
  geom_vline(xintercept = 8.5, color = "coral4")

# B - LDA 
library(MASS)
lda.reg <- lda(sys.df[,1:2], sys.df[,3])
summary(lda.reg)

pred.df <- predict(lda.reg, sys.df[,1:2], type="raw")

df <- data.frame(actual = sys.df$Completed.task, predicted = pred.df)
library(caret)
confusionMatrix(as.factor(df$actual), as.factor(df$predicted.class))

# C predict when experience = 4 and training = 6
test.df <- data.frame(Experience = 4, Training = 6)
predict(lda.reg, test.df, type="raw")

# how much experiences is required by admin with 4 training credits to be classified as yes
test2.df <- data.frame(Experience = seq(0, 20, 0.5), Training = rep(4, 41))
predict(lda.reg, test2.df, type="raw")

# logistic regression 
library(plyr)
sys.df$Completed.task <- as.factor(revalue(sys.df$Completed.task, c("Yes" = "1", "No" = "0")))
logit.reg <- glm(Completed.task ~ ., data = sys.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)
pred <- predict(logit.reg, sys.df, type = "response")

library(caret)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(sys.df$Completed.task))  
