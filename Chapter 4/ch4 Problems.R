##############################################################
# Problem 4.1 
############################################################
cereals.df <- read.csv("Cereals.csv") 

# a 
summary(cereals.df)
dim(cereals.df)
str(cereals.df)

# B Summary data frame 

cereals.num.df <- na.omit(cereals.df[,-c(1:3)])

data.frame(mean=sapply(cereals.num.df, mean), 
           sd=sapply(cereals.num.df, sd), 
           min=sapply(cereals.num.df, min), 
           max=sapply(cereals.num.df, max), 
           median=sapply(cereals.num.df, median), 
           length=sapply(cereals.num.df, length),
           miss.val=sapply(cereals.num.df, function(x) 
             sum(length(which(is.na(x))))))


# C Histogram 

# get the column names 
labels = colnames(cereals.num.df)

len <- length(labels)

# set the number of columns you want in the plot 
width <- 2
# perform len/width plus the modulo operation so that you always have the right number of rows 
par(mfrow = c(width, (len/width) + (len %% width)))

for (var in 1:len){
  hist(cereals.num.df[,var], xlab = labels[var], main = labels[var])
}

# reset graphing window 
par(mfrow = c(1,1))

# D 
library(ggplot2)
ggplot(cereals.df) + geom_boxplot(aes(x = as.factor(type), y = calories)) + xlab("Type of cereal")

# E 
ggplot(cereals.df) + geom_boxplot(aes(x = as.factor(shelf), y = rating)) + xlab("Shelf")

# F 

# correlation matrix 
cor(cereals.num.df)

library(gplots)
# heatmap with values 
heatmap.2(cor(cereals.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(cereals.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#normalize data 
cereals.norm.num.df <- data.frame(scale(cereals.num.df))
cor(cereals.norm.num.df)
heatmap.2(cor(cereals.norm.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(cereals.norm.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# They are the same

# G 

# principal components 
pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)
pcs.cor$rot[,1:5]

#####################################################################
# Problem 4.2 
########################################################################
uni.df <- read.csv("Universities.csv") 

uni.num.df <- na.omit(uni.df[,-c(1:3)])

library(gplots)

# heatmap with values 
heatmap.2(cor(uni.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(uni.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#remove redundant values.
uni.num.df <- uni.num.df[,-c(1,3,5,9)]

# see how the heatmap has changed 
heatmap.2(cor(uni.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(uni.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#perform PCA on on unscaled data 
pcs.cor <- prcomp(uni.num.df, scale. = T)
summary(pcs.cor)
#breakdown of top 5 components 
pcs.cor$rot[,1:5]

#perform PCA on scaled data 
uni.norm.num.df <- data.frame(scale(uni.num.df))
pcs.norm.cor <- prcomp(uni.norm.num.df, scale. = T)
summary(pcs.norm.cor)
pcs.norm.cor$rot[,1:5]

#####################################################################
# Problem 4.3
########################################################################

# D 
cars.df <- read.csv("ToyotaCorolla.csv") 
str(cars.df)

library(dummies)
cars.dum.df <- dummy.data.frame(cars.df, names = c("Mfg_Year","Color"), sep = ".")

# E 

# Keep only numeric 
cars.num.df <- na.omit(cars.df[,c(3,4,5,7,9,13,14, 16:18)])
str(cars.num.df)
# convert int to numeric 
cars.num.df[] <- lapply(cars.num.df, function(x) {
  if(is.integer(x)) x<-as.numeric(x) else x
})

sapply(cars.num.df, class)

heatmap.2(cor(cars.num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(cars.num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))
str(cars.num.df)

#####################################################################
# Problem 4.4
########################################################################
wine.df <- read.csv("Wine.csv")
pcs.cor <- prcomp(wine.df[,-1])
summary(pcs.cor)
pcs.cor$rot[,1:4]

str(wine.df)
wine.df[] <- lapply(wine.df, function(x) {
  if(is.integer(x)) x<-as.numeric(x) else x
})

# b 
# now do it again, but properly this time 
# aka normalize it 
#take the whole data set except the first column and normalize it 
wine.norm.df <- data.frame(scale(wine.df[,-c(1)]))

pcs.norm.cor <- prcomp(wine.norm.df)
summary(pcs.norm.cor)
pcs.norm.cor$rot[,1:4]
