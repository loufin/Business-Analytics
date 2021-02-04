

toyota.df <- read.csv("ToyotaCorolla.csv", header = TRUE)  # load data
dim(toyota.df)  # find the dimension of data frame
head(toyota.df)  # show the first six rows
View(toyota.df)  # show all the data in a new tab

str(toyota.df)

library(dummies)

toyotaFuelType.df <- toyota.df[c(1,2,8)]
str(toyotaFuelType.df)
toyotaFuelType.df$Fuel_Type <- as.factor(toyotaFuelType.df$Fuel_Type)

str(toyotaFuelType.df)

toyotaFuelType.df <- dummy.data.frame(toyotaFuelType.df, sep = ".", names=c("Fuel_Type","Color"))
names(toyotaFuelType.df)
