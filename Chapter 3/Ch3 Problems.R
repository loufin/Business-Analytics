
# we'll use this later when doing for loops of charts 
color_list = c("black", 
          "blue4",  
          "aquamarine3",
          "coral4",
          "cyan4",
          "darkgreen",
          "darkorange3",
          "grey48",
          "plum3")

##################################################################
# Problem 3.1 
##################################################################
shipment.df <- read.csv("ApplianceShipments.csv")

# use time series analysis
library(forecast)
shipments.ts <- ts(shipment.df$Shipments, start = c(1985, 1), end = c(1989, 4), freq = 4)
#freq =12 means that there's 12 data points per year, so monthly 
# Answer to A 
plot(shipments.ts, xlab = "Year", ylab = "Shipments", main = "Shipments per Quarter")
grid()


# B 
# in the word doc

# C 
quarter = c(1,2,3,4)

for (count in quarter) {
  #create variable to capture all first quarter rows (1,5,9,)
  quar <- seq(count, 20, by = 4)
  shipmentQ.df <- shipment.df[quar,]
  shipmentQ.ts <- ts(shipmentQ.df$Shipments, start=1985, end = 1989, freq = 1)
  if ( count > 1){
    lines(shipmentQ.ts, col = color_list[count], lwd=2)
  }
  else{
    plot(shipmentQ.ts, xlab = "Year", ylab = "Shipments", col = color_list[count] , ylim = c(3900,5000), lwd=2)
  }
  
}
grid()
legend(1985, 5000, legend = c("Q1","Q2","Q3","Q4"), col = c(color_list[1], color_list[2], color_list[3], color_list[4]), lty=1, cex=1)

# D 
# create yearly aggregate 
year = c(1985, 1986, 1987, 1988, 1989)
yearly.df <- data.frame("Year", "Shipments")
names(yearly.df) <- c("Year", "Shipments")
yearly.df <- yearly.df[0,]

for(count in 1:length(year)){
  # find what year we're in to aggregate 
  yearly <- seq ((count-1)*4 + 1, count*4 , by = 1)
  
  # create dataframe of rows for a particular year 
  yearlyShipment.df <- shipment.df[yearly,]
  
  print(yearlyShipment.df)
  #sum the year's shipments 
  agg = sum(yearlyShipment.df$Shipments)
  print(agg)
  # create temporary data frame based on yearly.df, but empty
  temp.df <- yearly.df[0,]
  
  # add tp that dataframe 
  temp.df[nrow(temp.df) + 1,] = list(year[count],agg)
  #add rows to the data frame
  yearly.df <- rbind(temp.df, yearly.df )
}

yearly.df <- yearly.df[order(yearly.df$Year),]
aggShipments.ts <- ts(yearly.df$Shipments ,start=1985, end = 1989, freq = 1)
plot(aggShipments.ts, xlab = "Year", ylab = "Shipments", main = "Aggregate Shipments per Year")
grid()

###########################################3
# Problem 3.2 
############################################
mowers.df <- read.csv("RidingMowers.csv")
library(ggplot2)

ggplot(mowers.df, aes(y = Lot_Size, x = Income, colour= Ownership)) +
  geom_point(alpha = 1) 

#############################################3
# Problem 3.3
#############################################

sales.df <- read.csv("LaptopSalesJanuary2008.csv")
summary(sales.df)
str(sales.df)
unique(sales.df$OS.X.Store)
unique(sales.df$OS.Y.Store)
# there are 15 unique store loactions
# let's get rid of unnecessary columns and just use the locations and price 

storeSales.df <- sales.df[c(5,15)]
# Since each store has unique pair of X and Y, we can just use one column
colnames(storeSales.df)[2] <- c("Store")
storeSales.df <- na.omit(storeSales.df)
stores <- c("Store1","Store2","Store3",
            "Store4","Store5","Store6",
            "Store7","Store8","Store9",
            "Store10","Store11","Store12",
            "Store13","Store14","Store15",
            "Store16","Store17"
            )
# need to create a list of all the unique names, as well as create new dataframe
uniqueStores <- unique(storeSales.df$Store)
averageSale.df <- storeSales.df[0,]

for(i in 1:length(uniqueStores)){
  sum <- 0
  count <- 0
  avg <- 0
  for(j in 1:length(storeSales.df$Store)){
    if( (uniqueStores[i]) == storeSales.df$Store[j]){
      storeSales.df$Store[j] <- stores[i]
      sum <- sum + storeSales.df$Retail.Price[j]
      count <- count +1
    }
  }
  avg <- sum/count
  temp.df <- data.frame(avg, stores[i])
  names(temp.df) <- c("Retail.Price", "Store")
  averageSale.df <- rbind(averageSale.df, temp.df )
}

# A 
# let's make a bar graph 
averageSale.df$Store <- as.factor(averageSale.df$Store)
storeSales.df$Store <- as.factor(storeSales.df$Store)
library(ggplot2)
ggplot(averageSale.df) + 
  geom_bar(aes(x = Store, y = Retail.Price), stat = "identity") +
  coord_cartesian(ylim = c(470,500) )

# Let's make a side by side boxplot 
ggplot(storeSales.df) + geom_boxplot(aes(x = as.factor(Store), y = Retail.Price)) + xlab("Store")

##############################################3
# Problem 3.4 
################################################3
sales.df <- read.csv("LaptopSales.csv")
head(sales.df)
summary(sales.df)
