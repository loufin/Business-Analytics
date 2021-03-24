bank.df <- read.csv("UniversalBank.csv")
head(bank.df)
str(bank.df)

train.index <- sample(bank.df$ID, 0.6*dim(bank.df)[1])
valid.index <- setdiff(bank.df$ID, train.index)

train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]

#(a)
bank1.df <- bank.df
for (i in 1:dim(bank1.df)[1]) 
{
  if (bank1.df[i,13]==0) bank1.df[i,13] = "notOnline"
  else bank1.df[i,13] = "Online"
  
  if (bank1.df[i,14]==0) bank1.df[i,14] = "noCreditCard"
  else bank1.df[i,14] = "hasCreditCard"
  
  if (bank1.df[i,10]==0) bank1.df[i,10] = "rejectedLoanOffer"
  else bank1.df[i,10] = "acceptedLoanOffer"
}

bank1.df$Online <- as.factor(bank1.df$Online)
bank1.df$CreditCard <- as.factor(bank1.df$CreditCard)
bank1.df$Personal.Loan <- as.factor(bank1.df$Personal.Loan)

library(pivottabler)
bank.pt <- qhpvt(bank1.df, c("CreditCard", "Personal.Loan"), "Online", "n()")
bank.pt

#(b)
# n(CC=1, online=1) = 882
# n(CC=1, online=1, Loan=1) = 82
# P(Loan=1 | CC=1, Online=1) = 9.30%

#(c)
###########
#...
#...


#############################

#8.2

accidents.df <- read.csv("AccidentsFull.csv")
names(accidents.df)[1] <- "HOUR_I_R"

accidents1.df <- accidents.df[,-24]
accidents1.df$Injury <- 0

for (i in 1:dim(accidents1.df)[1]) 
{
  if (accidents.df[i,24]==0) accidents1.df[i,24] = 0
  else if (accidents.df[i,24]==1) accidents1.df[i,24] = 1
  else accidents1.df[i,24] = 1
}

#(c)

accidents1.df <- accidents1.df[,c(1,3,5,6,7,8,9,11,12,17,24)]

accidents1.df$HOUR_I_R <- as.factor(accidents1.df$HOUR_I_R)
accidents1.df$ALIGN_I <- as.factor(accidents1.df$ALIGN_I)
accidents1.df$WRK_ZONE <- as.factor(accidents1.df$WRK_ZONE)
accidents1.df$WKDY_I_R <- as.factor(accidents1.df$WKDY_I_R)
accidents1.df$INT_HWY <- as.factor(accidents1.df$INT_HWY)
accidents1.df$LGTCON_I_R <- as.factor(accidents1.df$LGTCON_I_R)
accidents1.df$MANCOL_I_R <- as.factor(accidents1.df$MANCOL_I_R)
accidents1.df$RELJCT_I_R <- as.factor(accidents1.df$RELJCT_I_R)
accidents1.df$REL_RWY_R <- as.factor(accidents1.df$REL_RWY_R)
accidents1.df$TRAF_WAY <- as.factor(accidents1.df$TRAF_WAY)
accidents1.df$Injury <- as.factor(accidents1.df$Injury)

str(accidents1.df)

train.index <- sample(row.names(accidents1.df), 0.6*dim(accidents1.df)[1])
valid.index <- setdiff(row.names(accidents1.df), train.index)

train.df <- accidents1.df[train.index,]
valid.df <- accidents1.df[valid.index,]

library(e1071)
accidents.nb <- naiveBayes(Injury ~ ., data = train.df)
accidents.nb

## predict probabilities
pred.prob <- predict(accidents.nb, newdata = valid.df, type = "raw")
## predict class membership
pred.class <- predict(accidents.nb, newdata = valid.df)

df <- data.frame(actual = valid.df$Injury, predicted = pred.class)
df$falsePred <- abs(as.numeric(df$actual)-as.numeric(df$predicted))

sum(df$falsePred)*100/dim(df)[1]
