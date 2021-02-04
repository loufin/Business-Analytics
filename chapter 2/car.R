car.df <- read.csv("ToyotaCorolla.csv", header=TRUE)
View(car.df)

car1.df <- subset(car.df, select=-Model)
View(car1.df)

#method-1 for dummies
car2.df <- fastDummies::dummy_cols(car1.df)
View(car2.df)

#method-2 for dummies
car2a <- model.matrix(~0 + Fuel_Type + Color, data=car1.df)
car2a.df <- as.data.frame(car2a)
View(car2a.df)
# in this method, only the variables selected will be included in the data frame
# which in this case means Fuel_Type and Color variables. This can be either 
# convenient or inconvenient depending on what you are trying to do.

train.rows <- sample(rownames(car2.df), dim(car2.df)[1]*0.5)
train.data <- car2.df[train.rows, ]

rest.rows <- setdiff(rownames(car2.df), train.rows)
rest.data <- car2.df[rest.rows, ]

valid.rows <- sample(rownames(rest.data), dim(rest.data)[1]*0.6)
valid.data <- car2.df[valid.rows, ]

test.rows <- setdiff(rownames(rest.data), valid.rows)
test.data <- car2.df[test.rows, ]

View(train.data)
View(valid.data)
View(test.data)


