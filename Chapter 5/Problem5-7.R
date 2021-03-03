prop = c(0.03, 0.52, 0.38, 0.82, 0.33, 0.42, 0.55, 
         0.59, 0.09, 0.21, 0.43, 0.04, 0.08, 0.13, 
         0.01, 0.79, 0.42, 0.29, 0.08, 0.02)

act =  c(0, 0, 0, 1, 0, 0, 1, 
         0, 0, 0, 0, 0, 0, 0,
         0, 1, 0, 0, 0, 0)
cutoff_25 = act
cutoff_50 = act
cutoff_75 = act
library(caret)
df <- data.frame(prop, act, cutoff_25, cutoff_50, cutoff_75)

for (var in 1:length(prop)){
  if(df$prop[var] >= .25) df$cutoff_25[var] <- 1 else df$cutoff_25[var] <- 0
}
for (var in 1:length(prop)){
  if(df$prop[var] >= .5) df$cutoff_50[var] <- 1 else df$cutoff_50[var] <- 0
}
for (var in 1:length(prop)){
  if(df$prop[var] >= .75) df$cutoff_75[var] <- 1 else df$cutoff_75[var] <- 0
}

table(df$act, df$cutoff_25)
table(df$act, df$cutoff_50)
table(df$act, df$cutoff_75)

accuracy(df$act, df$cutoff_25)
accuracy(df$act, df$cutoff_50)
accuracy(df$act, df$cutoff_75)


library(gains)
lift = gains(actual = act, predicted = prop)
barplot(lift$mean.resp/mean(prop), names.arg = lift$depth, 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

