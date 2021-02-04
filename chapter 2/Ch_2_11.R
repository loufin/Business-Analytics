library(dummies)

car.df <- read.csv("ToyotaCorolla.csv", header=TRUE)

# A: Make dummy variables of Color and fuel Type 

#change variables Fuel Type and color into multicolumn binary variables 
car.df <- dummy.data.frame(car.df, sep = ".", names=c("Fuel_Type", "Color"))

# B: Partition the dataset into train, validate, and test 
set.seed(1)

#Set the training rows to be 50% of the dataset
train.rows <- sample(rownames(car.df), dim(car.df)[1]*0.5)

#set the validation rows to be 50% of the dataset, excluding the training rows 
valid.rows <- sample(setdiff(rownames(car.df), train.rows), 
                     dim(car.df)[1]*0.3)

# set the test rows to be be the remaining rows, excluding training and validation rows 
test.rows <- setdiff(rownames(car.df), union(train.rows, valid.rows))
                     
train.data <- car.df[train.rows, ]
valid.data <- car.df[valid.rows, ]
test.data <- car.df[test.rows, ]

