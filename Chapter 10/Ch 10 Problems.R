#################################################
# Problem 10.2 
#################################################

# A - create scatter plot of experience vs training 
sys.df <- read.csv("SystemAdministrators.csv")

library(ggplot2)
ggplot(sys.df, aes(y = Experience, x = Training, colour = Completed.task)) +
  geom_point(alpha = 1) 


# B do logistic regression on entire dataset 

sys.df$Completed.task <- as.factor(sys.df$Completed.task)
logit.reg <- glm(Completed.task ~ ., data = sys.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

library(plyr)

sys.df$Completed.task <- revalue(sys.df$Completed.task, c("Yes" = "1", "No" = "0"))
pred <- predict(logit.reg, sys.df, type = "response")

library(caret)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(sys.df$Completed.task))                

sys.df$predicted <- pred
