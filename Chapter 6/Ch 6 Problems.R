############################################################
# Problem 6.1 
############################################################
# Part B 
housing.df <- read.csv("BostonHousing.csv")

housing.df <- housing.df[,-c(14)]
set.seed(1)
train.rows <- sample(seq_len(nrow(housing.df)), size = floor(0.6 * nrow(housing.df)))

train.data <- housing.df[train.rows, ]
validation.data <- housing.df[-train.rows, ]

house.lm <- lm(MEDV ~ CRIM + CHAS + RM, data = train.data)

# D 

str(housing.df)

library(gplots)
# heatmap with values 
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(housing.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# D part iii 
house.lm <- lm(MEDV ~. ,data = train.data)
library(forecast)
# BACKWARDS
house.back.lm <- step(house.lm, direction = "backward")
summary(house.back.lm)  # Which variables were dropped?
house.back.lm.pred <- predict(house.back.lm, validation.data)
accuracy(house.back.lm.pred, validation.data$MEDV)

# FORWARD 
# create model with no predictors
house.null.lm <- lm(MEDV~1, data = train.data)
house.forward.lm <- step(house.null.lm, scope=list(lower=house.null.lm, upper=house.lm), direction = "forward")
summary(house.forward.lm)  # Which variables were added?
house.forward.lm.pred <- predict(house.forward.lm, validation.data)
accuracy(house.forward.lm.pred, validation.data$MEDV)

# BOTH
house.both.lm <- step(house.lm, direction = "both")
summary(house.both.lm)  # Which variables were dropped/added?
house.both.lm.pred <- predict(house.both.lm, validation.data)
accuracy(house.both.lm.pred, validation.data$MEDV)

############################################################
# Problem 6.2
############################################################
# A 
tayko.df <- read.csv("Tayko.csv")

# Categorical variables - web, gender, US, address_is_res 
# need to convert the nums into factors 
library(plyr)

#need a separate dataframe to store categorical, because we need the numericals version later 
tayko.cat.df <- tayko.df
tayko.cat.df$Gender.male <- as.factor(tayko.cat.df$Gender.male)
tayko.cat.df$Gender.male <- revalue(tayko.cat.df$Gender.male, c("1" = "Male", "0" = "Female"))

tayko.cat.df$Web.order <- as.factor(tayko.cat.df$Web.order)
tayko.cat.df$Web.order <- revalue(tayko.cat.df$Web.order, c("1" = "Purchased online at least once", "0" = "Not purchased online"))

tayko.cat.df$US <- as.factor(tayko.cat.df$US)
tayko.cat.df$US <- revalue(tayko.cat.df$US, c("1" = "US", "0" = "Not in US"))

tayko.cat.df$Address_is_res <- as.factor(tayko.cat.df$Address_is_res)
tayko.cat.df$Address_is_res <- revalue(tayko.cat.df$Address_is_res, c("1" = "Residential", "0" = "Non-residential"))

# Pivot table - method 1 
library(pivottabler)
pt <- PivotTable$new()
pt$addData(tayko.cat.df)
pt$addColumnDataGroups("Gender.male")
pt$addColumnDataGroups("Web.order")
pt$addRowDataGroups("US") 
pt$addRowDataGroups("Address_is_res") 
pt$defineCalculation(calculationName="Spending", summariseExpression="n()")
pt$renderPivot()

# pivot table - method 2 
# for mean, standard deviation 
qhpvt(tayko.cat.df, columns = c("Gender.male", "Web.order"), rows = c("US", "Address_is_res"),
      c("Mean Spending"="mean(Spending, na.rm=TRUE)"))
qhpvt(tayko.cat.df, columns = c("Gender.male", "Web.order"), rows = c("US", "Address_is_res"),
      c("Std Dev Spending"="sd(Spending, na.rm=TRUE)"))

# B 
library(ggplot2)
ggplot(tayko.df, aes(x=Freq, y=Spending)) +
  geom_point()

ggplot(tayko.df, aes(x=last_update_days_ago, y=Spending)) +
  geom_point()

# C 
set.seed(1)
# i 
train.rows <- sample(seq_len(nrow(tayko.df)), size = floor(0.9 * nrow(tayko.df)))

train.data <- tayko.df[train.rows, ]
validation.data <- tayko.df[-train.rows, ]

tayko.lm <- lm(Spending ~ Gender.male + Web.order + US + Address_is_res + Freq + last_update_days_ago, data = train.data)
# ii & iii
summary(tayko.lm)

# iv
tayko.back.lm <- step(tayko.lm, direction = "backward")
summary(tayko.back.lm)  # Which variables were dropped?
tayko.back.lm.pred <- predict(tayko.back.lm, validation.data)
accuracy(tayko.back.lm.pred, validation.data$Spending)

# v - use equation from part iii

# vi - use ME, RMSE, MAPE, 

# - vii 
# add predicted to the original df 
tayko.df$predictedSpending <- predict(tayko.lm,newdata=tayko.df)

library(ggplot2)
ggplot(tayko.df, aes(x = sequence_number)) +
  geom_point(aes(y=abs(Spending-predictedSpending)/Spending*100),color="blue") + ylab("% error")

############################################################
# Problem 6.3
############################################################
# a 
airfare.df <- read.csv("Airfares.csv")

#only numerical df for heatmap 
airfare.num.df <- airfare.df[,-c(1:4, 7,8,14,15)]
library(gplots)
# heatmap with values 
heatmap.2(cor(airfare.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(airfare.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# B 
#need to convert variable to have more than yes/no
airfare.df$VACATION <- revalue(airfare.df$VACATION, c("Yes" = "Vacation", "No" = "Non-vacation"))
airfare.df$SW <- revalue(airfare.df$SW, c("Yes" = "SW", "No" = "Non-SW"))

library(pivottabler)
pt <- PivotTable$new()
pt$addData(airfare.df)
pt$addColumnDataGroups("VACATION")
pt$addColumnDataGroups("SLOT")
pt$addRowDataGroups("SW") 
pt$addRowDataGroups("GATE") 
pt$defineCalculation(calculationName="Count", summariseExpression="n()")
pt$renderPivot()

qhpvt(airfare.df, columns = c("VACATION", "SLOT"), rows = c("SW", "GATE"),
      c("Mean FARE"="mean(FARE, na.rm=TRUE)"))

# C 
# i 
library(dummies)
airfare.df <- airfare.df[,-c(1:4)]
airfare.df <- dummy.data.frame(data = airfare.df, names = c("SW", "VACATION", "GATE","SLOT") , sep = ".")

train.rows <- sample(seq_len(nrow(airfare.df)), size = floor(0.9 * nrow(airfare.df)))

train.data <- airfare.df[train.rows, ]
validation.data <- airfare.df[-train.rows, ]

# ii 
library(forecast)
airfare.lm <- lm(FARE ~ ., data = train.data)

airfare.back.lm <- step(airfare.lm, direction = "backward")
summary(airfare.back.lm)  # Which variables were dropped?
airfare.back.lm.pred <- predict(airfare.back.lm, validation.data)
accuracy(airfare.back.lm.pred, validation.data$FARE)

# iii - exhuastive search 
library(leaps)
airfare.search <- regsubsets(FARE ~ ., data = train.data,
                     method = "exhaustive")
summary(airfare.search)
sum <- summary(airfare.search)

# show models
sum$which
# show metrics
sum$rsq # -this is the big one 
sum$adjr2
sum$Cp

# iv 
sum$rsq
accuracy(airfare.back.lm)

# v 
# do some math 

# vi 
 # look at the pivot table 

# vii
# In text 

# viii
# build mode with only info available before flight 
# 

# ix
library(leaps)
airfare.search <- regsubsets(FARE ~ S_INCOME + E_INCOME + E_POP + S_POP + DISTANCE, data = train.data,
                             method = "exhaustive")
summary(airfare.search)
sum <- summary(airfare.search)
sum$rsq
# x 
