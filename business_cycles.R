# R Programme to estimate trend and cyclic components of 
# macroeconomic variables
# By Eliab G. Luvanda
# October 2015
rm(list = ls())
# Loading required packages
library(mFilter)

# Setting working dirctory
setwd("~/tra_business_cycle")

# Opening data file
macp1 <- read.csv("macp1.csv")
# View(macp1)
attach(macp1)

y <- ts(lgdp,start=1952,end=2010)
x <- ts(inf,start=1952,end=2010)

# Plotting variables on graph
plot.ts(y,ylab = "Log of GDP", main = "Real GDP")
plot.ts(x,ylab = "Percent",main = "Rate of Inflation")

# Estimation of trend and cyclic components using the HP filter
# Real GDP
gdp <- hpfilter(y,type = "lambda")
plot(gdp$cycle, ylab = "",main = "Cyclic Component of Real GDP")
 
plot(gdp$trend,ylab = "", main = "Actual and Trend Component of Real GDP")
lines(y)

# Inflation
infl <- hpfilter(x,type = "lambda")
plot(infl$cycle, ylab = "",main = "Cyclic Component of Inflation")

plot(infl$trend,ylab = "", main = "Actual and Trend Component of Inflation")
lines(x)


# Estimation of trend and cyclic components using the bk filter
# Real GDP
gdpb <- bkfilter(y)
plot(gdpb$cycle, ylab = "",main = "Cyclic Component of Real GDP")

plot(gdpb$trend,ylab = "", main = "Actual and Trend Component of Real GDP")
lines(y)

# Inflation
inflb <- hpfilter(x,type = "lambda")
plot(inflb$cycle, ylab = "",main = "Cyclic Component of Inflation")

plot(inflb$trend,ylab = "", main = "Actual and Trend Component of Inflation")
lines(x)


