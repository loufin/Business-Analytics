# 12.1

bank.df <- read.csv("UniversalBank.csv")
#View(head(bank.df))
str(bank.df)

bank.df <- bank.df[,-5]

bank.df <- bank.df[,c(1:8,10:13,9)]

train.index <- sample(bank.df$ID, 0.6*dim(bank.df)[1])
valid.index <- setdiff(bank.df$ID, train.index)

train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]

#LDA using DiscriMiner
library(DiscriMiner)
da.reg <- linDA(train.df[,1:12], train.df[,13])
da.reg$functions
summary(da.reg)

# LDA using MASS
# the advantage of MASS is that you can use the predict function
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