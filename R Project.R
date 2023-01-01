#Data set taken from Kaggle.com -- https://www.kaggle.com/datasets/samuelcortinhas/house-price-prediction-seattle
#From the user Samuel Cortinhas
#"This is a real dataset of house prices sold in Seattle, Washing, USA between 
#August and December 2022. The task is to predict the house price in this area based on several features, 
#which are described below."

"Here we are uploading the data into 'mydata', and then creating a dataframe from 'mydata'"
mydata = read.csv("/Users/macbookpro/Downloads/archive (3)/train.csv")
df = data.frame(mydata)
print(df)

#adding necessary data packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(explore)
library(dplyr)

#Dropping unneeded columns from df
df = subset(df, lot_size_units == 'sqft')
df = select(df, -4, -6)
df

#Renaming columns for easy access
beds = df$beds
zipcode = df$zip_code
lot = df$lot_size
price = df$price
baths = df$baths
property.size = df$size

#sum of unique zipcodes and how many were sold in that area
table(df['zip_code'])

uzipcode = c(98102, 98103, 98104, 98105, 98106, 98107, 98108, 98109, 98112, 98115, 98116, 98117, 98118, 98119, 98122,
             98125, 98126, 98133, 98136, 98144, 98146, 98168, 98177, 98178, 98199)
unums = c(24, 132, 1, 57, 71, 69, 33, 27, 43, 127, 61, 140, 83, 39, 80, 62, 62, 38, 52, 87, 28, 27, 24, 28, 54)

barplot(unums, uzipcode, main="Number of Homes Sold vs Zipcodes", horiz=TRUE, las=1, xlab = "Number of Homes sold", ylab = "Zipcodes",
        names.arg=c("98102", "98103", "98104", "98105", "98106", "98107", "98108", "98109", "98112", "98115", "98116", "98117", "98118", "98119", "98122",
                    "98125", "98126", "98133", "98136", "98144", "98146", "98168", "98177", "98178", "98199"))

#Unique values of certain columns
unique(df$zip_code)
unique(df$beds)
unique(df$baths)
str(df)

plot(beds, baths, main = "Beds and Bath")

#Sorting the df in ascending order by zipcode, while maintaing a descending price 
#in the same line code.
df = df[order(df$zip_code, -df$price), ]
df

#seperating each of the zipcodes -- Feels like this is an easier way to do this.
unique(df$zip_code)
df98102 = df[df$zip_code %in% "98102", ]
df98103 = df[df$zip_code %in% "98103", ]
df98104 = df[df$zip_code %in% "98104", ]
df98105 = df[df$zip_code %in% "98105", ]
df98106 = df[df$zip_code %in% "98106", ]
df98107 = df[df$zip_code %in% "98107", ]
df98108 = df[df$zip_code %in% "98108", ]
df98109 = df[df$zip_code %in% "98109", ]
df98112 = df[df$zip_code %in% "98112", ]
df98115 = df[df$zip_code %in% "98115", ]
df98116 = df[df$zip_code %in% "98116", ]
df98117 = df[df$zip_code %in% "98117", ]
df98118 = df[df$zip_code %in% "98118", ]
df98119 = df[df$zip_code %in% "98119", ]
df98122 = df[df$zip_code %in% "98122", ]
df98125 = df[df$zip_code %in% "98125", ]
df98126 = df[df$zip_code %in% "98126", ]
df98133 = df[df$zip_code %in% "98133", ]
df98136 = df[df$zip_code %in% "98136", ]
df98144 = df[df$zip_code %in% "98144", ]
df98146 = df[df$zip_code %in% "98146", ]
df98168 = df[df$zip_code %in% "98168", ]
df98177 = df[df$zip_code %in% "98177", ]
df98178 = df[df$zip_code %in% "98178", ]
df98199 = df[df$zip_code %in% "98199", ]

#Code to relay Bed v Prices, oma = out margin area
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98102$beds, df98102$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98102")

plot(df98103$beds, df98103$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98103")

plot(df98104$beds, df98104$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98104")

plot(df98105$beds, df98105$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98105")

plot(df98106$beds, df98106$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98106")

plot(df98107$beds, df98107$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98107")

plot(df98108$beds, df98108$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98108")

plot(df98109$beds, df98109$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98109")

plot(df98112$beds, df98112$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98104")

mtext("Bedrooms vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="olivedrab")

#For the next set
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98115$beds, df98115$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98115")

plot(df98116$beds, df98116$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98116")

plot(df98117$beds, df98117$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98117")

plot(df98118$beds, df98118$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98118")

plot(df98119$beds, df98119$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98119")

plot(df98122$beds, df98122$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98122")

plot(df98125$beds, df98125$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98125")

plot(df98126$beds, df98126$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98126")

plot(df98133$beds, df98133$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98133")

mtext("Bedrooms vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="olivedrab")

#Code for the last set
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98136$beds, df98136$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98136")

plot(df98144$beds, df98144$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98144")

plot(df98146$beds, df98146$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98146")

plot(df98168$beds, df98168$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98168")

plot(df98177$beds, df98177$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98177")

plot(df98178$beds, df98178$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "ZipCode 98178")

plot(df98199$beds, df98199$price, xlab = "Number of Beds", 
     ylab = "Price (USD$)",
     main = "Zipcode 98199")

mtext("Bedrooms vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="olivedrab")

#Code for relating property sizes to price
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98102$size, df98102$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98102")

plot(df98103$size, df98103$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98103")

plot(df98104$size, df98104$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98104")

plot(df98105$size, df98105$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98105")

plot(df98106$size, df98106$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98106")

plot(df98107$size, df98107$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98107")

plot(df98108$size, df98108$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98108")

plot(df98109$size, df98109$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98109")

plot(df98112$size, df98112$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98104")

mtext("Property Size vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="red")

#Code for the Second set
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98115$size, df98115$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98115")

plot(df98116$size, df98116$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98116")

plot(df98117$size, df98117$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98117")

plot(df98118$size, df98118$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98118")

plot(df98119$size, df98119$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98119")

plot(df98122$size, df98122$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98122")

plot(df98125$size, df98125$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98125")

plot(df98126$size, df98126$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98126")

plot(df98133$size, df98133$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98133")

mtext("Property Size vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="red")

#Code for the last set
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98136$size, df98136$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98136")

plot(df98144$size, df98144$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98144")

plot(df98146$size, df98146$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98146")

plot(df98168$size, df98168$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98168")

plot(df98177$size, df98177$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98177")

plot(df98178$size, df98178$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98178")

plot(df98199$size, df98199$price, xlab = "Property Size (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98199")

mtext("Property Size vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="red")

#Property Lot sizes vs Prices part 1
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98102$lot_size, df98102$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98102")

plot(df98103$lot_size, df98103$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98103")

plot(df98104$lot_size, df98104$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98104")

plot(df98105$lot_size, df98105$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98105")

plot(df98106$lot_size, df98106$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98106")

plot(df98107$lot_size, df98107$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98107")

plot(df98108$lot_size, df98108$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98108")

plot(df98109$lot_size, df98109$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98109")

plot(df98112$lot_size, df98112$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98104")

mtext("Property Lot Sizes vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="blue")

#Code for the Second set
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98115$lot_size, df98115$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98115")

plot(df98116$lot_size, df98116$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98116")

plot(df98117$lot_size, df98117$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98117")

plot(df98118$lot_size, df98118$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98118")

plot(df98119$lot_size, df98119$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98119")

plot(df98122$lot_size, df98122$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98122")

plot(df98125$lot_size, df98125$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98125")

plot(df98126$lot_size, df98126$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98126")

plot(df98133$lot_size, df98133$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98133")

mtext("Property Lot Sizes vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="blue")

#Code for the last set
par(mfrow=c(3,3), oma = c(0,0,2,0)) # puts 9 plots in one window (3x3)

plot(df98136$lot_size, df98136$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98136")

plot(df98144$lot_size, df98144$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98144")

plot(df98146$lot_size, df98146$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98146")

plot(df98168$lot_size, df98168$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98168")

plot(df98177$lot_size, df98177$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98177")

plot(df98178$lot_size, df98178$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "ZipCode 98178")

plot(df98199$lot_size, df98199$price, xlab = "Property Lot Sizes (sqft)", 
     ylab = "Price (USD$)",
     main = "Zipcode 98199")

mtext("Property Lot Sizes vs Prices of Homes in Seattle, Washington", outer=TRUE, cex = 1.5, col="blue")

#imported dataset created from researching income levels based on zipcodes
mydata2 = read.csv("/Users/macbookpro/Downloads/zipcode_and_income - Sheet1.csv")
dfincome = data.frame(mydata2)
dfincome

describe(dfincome)
plot(dfincome$Zip.Codes, dfincome$Income, xlab = "Zipcodes", 
     ylab = "Average Income Household",
     main = "Zipcodes vs Income Household")

#organize the data by income
dfincome = dfincome[order(-dfincome$Income), ]
dfincome

#Top 5 Zipcodes that make the most income and their demographic
par(fig=c(0,0.8,0,0.8), new=TRUE)
plot(df98102$beds, df98102$price, xlab="Number of Beds",
     ylab="Price (USD$)")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(df98102$beds, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(df98102$price, axes=FALSE)
mtext("Beds vs Price in Zipcode 98102", side=3, outer=TRUE, line=-3)

par(fig=c(0,1,0,1), new=TRUE)
plot(df98117$beds, df98117$price, xlab="Number of Beds",
     ylab="Price (USD$)")
par(fig=c(0,0.9,0.55,1), new=TRUE)
boxplot(df98117$beds, horizontal=TRUE, axes=FALSE)
par(fig=c(0.7,1.0,0,0.9),new=TRUE)
boxplot(df98117$price, axes=FALSE)
mtext("Beds vs Price in Zipcode 98117", side=3, outer=TRUE, line=-3)

par(fig=c(0,1,0,1), new=TRUE)
plot(df98177$beds, df98177$price, xlab="Number of Beds",
     ylab="Price (USD$)")
par(fig=c(0,0.9,0.55,1), new=TRUE)
boxplot(df98177$beds, horizontal=TRUE, axes=FALSE)
par(fig=c(0.7,1.0,0,0.9),new=TRUE)
boxplot(df98177$price, axes=FALSE)
mtext("Beds vs Price in Zipcode 98177", side=3, outer=TRUE, line=-3)

par(fig=c(0,1,0,1), new=TRUE)
plot(df98115$beds, df98115$price, xlab="Number of Beds",
     ylab="Price (USD$)")
par(fig=c(0,0.9,0.55,1), new=TRUE)
boxplot(df98115$beds, horizontal=TRUE, axes=FALSE)
par(fig=c(0.7,1.0,0,0.9),new=TRUE)
boxplot(df98115$price, axes=FALSE)
mtext("Beds vs Price in Zipcode 98115", side=3, outer=TRUE, line=-3)

par(fig=c(0,1,0,1), new=TRUE)
plot(df98116$beds, df98116$price, xlab="Number of Beds",
     ylab="Price (USD$)")
par(fig=c(0,0.9,0.55,1), new=TRUE)
boxplot(df98116$beds, horizontal=TRUE, axes=FALSE)
par(fig=c(0.7,1.0,0,0.9),new=TRUE)
boxplot(df98116$price, axes=FALSE)
mtext("Beds vs Price in Zipcode 98116", side=3, outer=TRUE, line=-3)

#Creating a easy to look at Shiny Model for HTML
df %>% explore()
df %>% 
  explore_all(
    target = price, 
    ncol = 2
  )

df %>%
  report(
    target = price, 
    output_dir = "/Users/macbookpro/Documents/R/",
    output_file = "explore_html"
  )
