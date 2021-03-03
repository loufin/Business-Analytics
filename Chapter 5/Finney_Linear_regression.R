cancer.df <- read.csv("cancer_reg.csv") 

cancer.df <- na.omit(cancer.df)

head(cancer.df)

# take top 30 rows to be able to view 
cancer.head.df <- cancer.df[1:30,]

str(cancer.df)

#Geography, binnedInc, avgDeathsperYear, medIncome, and popEst2015 are all non num 

# let's remove the chr variables 
cancer.num.df <- cancer.df[,-c(9,13)]

#now convert int vars to numeric
cancer.num.df[] <- lapply(cancer.num.df, function(x) {
  if(is.integer(x)) x<-as.numeric(x) else x
  
})

cor(cancer.num.df)

library(gplots)
# heatmap with values 
heatmap.2(cor(cancer.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(cancer.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#select variables to be used 
cancer.clean.df <- cancer.num.df[,c("avgAnnCount","incidenceRate","medIncome","popEst2015", "AvgHouseholdSize",
                       "povertyPercent","MedianAge","PctPrivateCoverage","PctPublicCoverage","BirthRate","avgDeathsPerYear")]

#check the heatmap
heatmap.2(cor(cancer.clean.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(cancer.clean.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


# create a normalized dataset, just to test later 
cancer.norm.clean.df <- data.frame(scale(cancer.clean.df))
#check the heatmap
heatmap.2(cor(cancer.norm.clean.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(cancer.norm.clean.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


library(forecast)

set.seed(1)
train.rows <- sample(seq_len(nrow(cancer.clean.df)), size = floor(0.6 * nrow(cancer.clean.df)))

train.data <- cancer.clean.df[train.rows, ]
test.data <- cancer.clean.df[-train.rows, ]

#build a lm 
reg <- lm(avgDeathsPerYear~., data = cancer.clean.df, subset = train.rows)
pred.train <- predict(reg)

pred.validation <- predict(reg, newdata=test.data)

## evaluate performance
# training
accuracy(pred.train, cancer.clean.df[train.rows,]$avgDeathsPerYear)
# validation
accuracy(pred.validation, cancer.clean.df[-train.rows,]$avgDeathsPerYear)

library(jtools)
summ(reg)

library(ggplot2)
ggplot(data1.df, aes(x = ID)) +
  geom_point(aes(y=abs(avgDeathsPerYear-predictedAvgDeath)/avgDeathsPerYear*100),color="blue") + ylab("% error")

#############################################################################
# Dr. Iqbal's code 
data.df <- read.csv("cancer_reg.csv",header = TRUE)
# data.df <- na.omit(data.df)
View(head(data.df, 30))
str(data.df)
data.df <- data.df[,-c(9,13)]

round(cor(data.df, use = "na.or.complete"),2)

str(data.df)
data1.df <- data.df[,c("avgAnnCount","avgDeathsPerYear","incidenceRate","medIncome","popEst2015",
                       "povertyPercent","MedianAge","PctPrivateCoverage","PctPublicCoverage","BirthRate","avgDeathsPerYear")]



for (i in c(1:length(data1.df$avgAnnCount)))
{
  data1.df$ID[i] <- i
}
View(data1.df)



library(forecast)



training <- sample(data1.df$ID, length(data1.df$ID)*0.6)
validation <- sample(setdiff(data1.df$ID, training), (length(data1.df$ID)*0.4))



# run linear regression model
reg <- lm(avgDeathsPerYear~., data=data1.df, subset=training,
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=data1.df[validation,],
                  na.action=na.pass)



## evaluate performance
# training
accuracy(pred_t, data1.df[training,]$avgDeathsPerYear)
# validation
accuracy(pred_v, data1.df[validation,]$avgDeathsPerYear)



data1.df$predictedAvgDeath <- predict(reg,newdata=data1.df)
View(data1.df)



library(ggplot2)
ggplot(data1.df, aes(x = ID)) +
  geom_point(aes(y=abs(avgDeathsPerYear-predictedAvgDeath)/avgDeathsPerYear*100),color="blue") + ylab("% error")
#+ geom_point(aes(y=popEst2015),color="red")