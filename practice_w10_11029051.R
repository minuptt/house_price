#read the ‘HousePrice.csv’,load it to a data frame.

setwd('/Users/ThiPhan1/Documents/UNT/1FALL2019/INFO3010/WEEK10')
house <- read.csv("HousePrice.csv")

str(house)
#head(house, 10)

#train a linear regression model: model the prie of the house based on sqrt_area, lot_area, age and crime data.
model <-lm(Price~Sqft_Area+Age+Lot_Area+Age+Crime, data = house)
model

#check how the model looks like model // *hypothesis ~ linear relation / non hypothesis ~ no linear relation*
summary(model)

