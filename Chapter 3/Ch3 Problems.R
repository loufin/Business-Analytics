

# Problem 3 
shipment.df <- read.csv("ApplianceShipments.csv")

# use time series analysis
library(forecast)
shipments.ts <- ts(shipment.df$Shipments, start = c(1985, 1), end = c(1989, 4), freq = 4)
#freq =12 means that there's 12 data points per year, so monthly 
plot(shipments.ts, xlab = "Year", ylab = "Shipments")

shipments.year.ts <- ts(shipment.df$Shipments, start = c(1985, 1), end = c(1985, 4), freq = 4)
plot(shipments.year.ts, xlab = "Year", ylab = "Shipments")
par(new=TRUE)
shipments.year.ts <- ts(shipment.df$Shipments, start = c(1986, 1), end = c(1986, 4), freq = 4)
plot(shipments.year.ts, col = "red")
year = c(1985, 1989)
quarter = c(1,4)


# C 
Q1 <- seq(1,20, by = 4)
Q2 <- seq(2,20, by = 4)
Q3 <- seq(3,20, by = 4)
Q4 <- seq(4,20, by = 4)

shipmentQ1.df <- shipment.df[Q1,]
shipmentQ2.df <- shipment.df[Q2,]
shipmentQ3.df <- shipment.df[Q3,]
shipmentQ4.df <- shipment.df[Q4,]

shipmentQ1.ts <- ts(shipmentQ1.df$Shipments, start=1985, end = 1989, freq = 1)
shipmentQ2.ts <- ts(shipmentQ2.df$Shipments, start=1985, end = 1989, freq = 1)
shipmentQ3.ts <- ts(shipmentQ3.df$Shipments, start=1985, end = 1989, freq = 1)
shipmentQ4.ts <- ts(shipmentQ4.df$Shipments, start=1985, end = 1989, freq = 1)

plot(shipmentQ1.ts, xlab = "Year", ylab = "Shipments", col = "blue", ylim = c(3900,5000))
lines(shipmentQ2.ts, col = "red")
lines(shipmentQ3.ts, col = "green")
lines(shipmentQ4.ts, col = "yellow")
legend(1985, 5000, legend = c("Q1","Q2","Q3","Q4"), col = c("blue", "red", "green", "yellow"), lty=1, cex=1)

