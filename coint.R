
rm(list = ls())
setwd("~/tra_business_cycle")
sink(file = "coint",append = FALSE)

# reading in data
rev_quart_r <- read.csv("rev_quart_r.csv")
gdp_quart <- read.csv("gdp_quart.csv")

# merging the two data frames
# rev_gdp <- merge(rev_quart_r, gdp_quart)

# creating time series objects
agri_r <- ts(log(rev_quart_r$agri_r), frequency = 4, start = c(2001,1))
agri <- ts(log(gdp_quart$agri), frequency = 4, start = c(2001,1))
mining_r <- ts(log(rev_quart_r$mining_r), frequency = 4, start = c(2001,1))
mining <- ts(log(gdp_quart$mining), frequency = 4, start = c(2001,1))
manufacturing_r <- ts(log(rev_quart_r$manufacturing_r), frequency = 4, 
                      start = c(2001,1))
manufacturing <- ts(log(gdp_quart$manufacturing), frequency = 4, start = c(2001,1))
electricity_r <- ts(log(rev_quart_r$electricity_r), frequency = 4, 
                    start = c(2001,1))
electricity <- ts(log(gdp_quart$electricity), frequency = 4, start = c(2001,1))
construction_r <- ts(log(rev_quart_r$construction_r), frequency = 4, 
                     start = c(2001,1))
construction <- ts(log(gdp_quart$construction), frequency = 4, start = c(2001,1))
trade_r <- ts(log(rev_quart_r$trade_r), frequency = 4, 
              start = c(2001,1))
trade <- ts(log(gdp_quart$trade), frequency = 4, start = c(2001,1))
transport_r <- ts(log(rev_quart_r$transport_r), frequency = 4, 
                  start = c(2001,1))
transport <- ts(log(gdp_quart$transport), frequency = 4, start = c(2001,1))
hotels_r <- ts(log(rev_quart_r$hotels_r), frequency = 4, 
               start = c(2001,1))
hotels <- ts(log(gdp_quart$hotels), frequency = 4, start = c(2001,1))
financial_r <- ts(log(rev_quart_r$financial_r), frequency = 4, 
                  start = c(2001,1))
financial <- ts(log(gdp_quart$financial), frequency = 4, start = c(2001,1))
estates_r <- ts(log(rev_quart_r$estates_r), frequency = 4, 
                start = c(2001,1))
estates <- ts(log(gdp_quart$estates), frequency = 4, start = c(2001,1))

education_r <- ts(log(rev_quart_r$education_r), frequency = 4, 
                  start = c(2001,1))
education <- ts(log(gdp_quart$education), frequency = 4, start = c(2001,1))
health_r <- ts(log(rev_quart_r$health_r), frequency = 4, 
               start = c(2001,1))
other_service <- ts(log(gdp_quart$other_service), frequency = 4, 
                    start = c(2001,1))
library(urca)
# Agriculture
za <- cbind(agri_r, agri)
coza <- ca.po(za, type = "Pz")
summary(coza)

# Mining
zmi <- cbind(mining_r, mining)
comi <- ca.po(zmi, type = "Pz")
summary(comi)

# Manufacturing
zma <- cbind(manufacturing_r, manufacturing)
coma <- ca.po(zma, type = "Pz")
summary(coma)

# Electricity
zel <- cbind(electricity_r, electricity)
coel <- ca.po(zel,  type = "Pz")
summary(coel)

# Construction
zco <- cbind(construction_r, construction)
coco <- ca.po(zco,  type = "Pz")
summary(coco)

# Trade
ztrd <- cbind(trade_r, trade)
cotrade <- ca.po(ztrd, type = "Pz")
summary(cotrade)

# Transport
ztras <- cbind(transport_r, transport)
cotrans <- ca.po(ztras,  type = "Pz")
summary(cotrans)

# Hotels
zhot <- cbind(hotels_r, hotels)
cohot <- ca.po(zhot, type = "Pz")
summary(cohot)

# Financial services
zfin <- cbind(financial_r, financial)
cofin <- ca.po(zfin, type = "Pz")
summary(cofin)

# Real estates
zest <- cbind(estates_r, estates)
coest <- ca.po(zest, type = "Pz")
summary(coest)

# Education
zedu <- cbind(education_r, education)
coedu <- ca.po(zedu, type = "Pz")
summary(coedu)

# Health
zhea <- cbind(health_r, other_service)
cohea <- ca.po(zhea, type = "Pz")
summary(cohea)
