######################################################
# Problem 8.1 
#####################################################
bank.df <- read.csv("UniversalBank.csv")

# A - Pivot table 
library(pivottabler)
library(plyr)


bank.df$CreditCard <- as.factor(bank.df$CreditCard)
bank.df$CreditCard <- revalue(bank.df$CreditCard, c("1" = "hasCC", "0" = "noCC"))

bank.df$Personal.Loan <- as.factor(bank.df$Personal.Loan)
bank.df$Personal.Loan <- revalue(bank.df$Personal.Loan, c("1" = "Loan", "0" = "noLoan"))

bank.df$Online <- as.factor(bank.df$Online)
bank.df$Online <- revalue(bank.df$Online, c("1" = "Online", "0" = "notOnline"))

# partition 60/40
set.seed(1)
train.rows <- sample(seq_len(nrow(bank.df)), size = floor(0.6 * nrow(bank.df)))
train.data <- bank.df[train.rows, ]
validation.data <- bank.df[-train.rows, ]

qhpvt(bank.df, columns = "CreditCard", rows = c("Personal.Loan","Online") ,calculations= "n()")

# b - P(Loan | CC, Online)
82/882 

# c - two pivot tables of training 
# one for row: Loan, column: online 
qhpvt(train.data, columns = "Personal.Loan", rows = "Online" ,calculations= "n()")
# one for row: Loan, column: CC 
qhpvt(train.data, columns = "Personal.Loan", rows = "CreditCard" ,calculations= "n()")
qhpvt(train.data, columns = "CreditCard", rows = c("Personal.Loan","Online") ,calculations= "n()")

# D bunch of subquestions 
#i P(CC | Loan)
P_cc_given_loan <- 77 / 275

# ii P (Online | Loan)
P_online_given_loan <- 166 / 275 

# iii P(Loan)
P_Loan <- 275 / 3000

# iv P(CC | noLoan)
P_cc_given_noLoan <- 198 / 275 

# v P( Online | noLoan)
P_online_given_noLoan <- 1588 / 2725

# vi P(noLoan)
P_noLoan <- 2725 / 3000

#  P(Loan|CC, Online) = (p (CC|Loan) * P(Online|Loan) * P (Loan)) / Bayes constant 
# Bayes Constant = P(Loan)* P(CC|Loan) * P(Online|Loan) 
  # +  P(noLoan) * P(noCC|noLoan)* P(Online|noLoan|)  
baye <- (P_Loan * P_cc_given_loan * P_online_given_loan) + 
  (P_noLoan * P_cc_given_noLoan * P_online_given_noLoan)

prob <- (P_Loan * P_cc_given_loan * P_online_given_loan)/baye

# F 
# answered in document 

# G 
library(e1071)
bank.nb <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = train.data)
bank.nb
pred.prob <- predict(bank.nb, newdata = validation.data, type = "raw")
# 0.09 for 1,1 

######################################################
# Problem 8.2 
#####################################################
# a 
accidents.df <- read.csv("AccidentsFull.csv")
names(accidents.df)[1] <- "HOUR_I_R"
accidents.df$INJURY_CRASH <- as.factor(accidents.df$INJURY_CRASH)
summary(accidents.df$INJURY_CRASH)

# b 
# first twelve rows 
acc.df <- accidents.df[1:12, c(16,19,20)]

acc.df$TRAF_CON_R <- as.factor(acc.df$TRAF_CON_R)
acc.df$WEATHER_R <- as.factor(acc.df$WEATHER_R)

acc.df$TRAF_CON_R <- revalue(acc.df$TRAF_CON_R, c("0" = "no_ctrl", "1" = "signal_ctrl", "2" = "other_ctrl"))
acc.df$INJURY_CRASH <- revalue(acc.df$INJURY_CRASH, c("0" = "No_injury", "1" = "Injury"))
acc.df$WEATHER_R <- revalue(acc.df$WEATHER_R, c("1" = "weather_fine", "2" = "weather_adverse"))

# i pivot table 
library(pivottabler)
qhpvt(acc.df, columns = "INJURY_CRASH", rows = c("WEATHER_R","TRAF_CON_R") ,calculations= "n()")

# ii 
#in document 

# iii classify 
# in document, all no injury 

#iv manually calculate P( INJURY | Weather_fine, signal_ctrl)
# 
numerator <- .25 * .667 * 0 
denom <- 1 
# denominator doesn't matter because we know the numerator will be 0
numerator/denom

# v 
library(e1071)
acc.nb <- naiveBayes(INJURY_CRASH ~ WEATHER_R + TRAF_CON_R, data = acc.df)
acc.nb
pred.prob <- predict(acc.nb, newdata = acc.df, type = "raw")
pred.prob
pred.class <- predict(acc.nb, newdata = acc.df)
pred.class

# C  
accidents.df <- accidents.df[,-c(18,21,23,24)]

library(dplyr)
accidents.df <- accidents.df %>% mutate_if(is.character,as.factor)
accidents.df <- accidents.df %>% mutate_if(is.numeric,as.factor)
str(accidents.df)

set.seed(1)
train.rows <- sample(seq_len(nrow(accidents.df)), size = floor(0.6 * nrow(accidents.df)))
train.data <- accidents.df[train.rows, ]
validation.data <- accidents.df[-train.rows, ]

# ii 
library(e1071)
accidents.nb <- naiveBayes(INJURY_CRASH ~ ., data = train.data)
accidents.nb
pred.prob <- predict(accidents.nb, newdata = validation.data, type = "raw")
pred.class <- predict(accidents.nb, newdata = validation.data)
library(caret)
confusionMatrix(pred.class, validation.data$INJURY_CRASH)

