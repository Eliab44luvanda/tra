
rm(list = ls())
setwd("~/tra_business_cycle")
sink(file = "rev_gdp",append = FALSE)

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



# decomposing time serie using hpfilter
require(mFilter)
# agriculture
agri_rf <- hpfilter(agri_r, freq = 4, type = "frequency")
agrif <- hpfilter(agri, freq = 4, type = "frequency")

# mining
mining_rf <- hpfilter(agri_r, freq = 4, type = "frequency")
miningf <- hpfilter(mining, freq = 4, type = "frequency")

# manufacturing
manufacturing_rf <- hpfilter(manufacturing_r, freq = 4, type = "frequency")
manufacturingf <- hpfilter(manufacturing, freq = 4, type = "frequency")

# electricity
electricity_rf <- hpfilter(electricity_r, freq = 4, type = "frequency")
electricityf <- hpfilter(electricity, freq = 4, type = "frequency")

# construction
construction_rf <- hpfilter(construction_r, freq = 4, type = "frequency")
constructionf <- hpfilter(construction, freq = 4, type = "frequency")

# trade
trade_rf <- hpfilter(trade_r, freq = 4, type = "frequency")
tradef <- hpfilter(trade, freq = 4, type = "frequency")

# transport
transport_rf <- hpfilter(transport_r, freq = 4, type = "frequency")
transportf <- hpfilter(transport, freq = 4, type = "frequency")

# hotels
hotels_rf <- hpfilter(hotels_r, freq = 4, type = "frequency")
hotelsf <- hpfilter(hotels, freq = 4, type = "frequency")

# financial
financial_rf <- hpfilter(financial_r, freq = 4, type = "frequency")
financialf <- hpfilter(financial, freq = 4, type = "frequency")

# estates
estates_rf <- hpfilter(estates_r, freq = 4, type = "frequency")
estatesf <- hpfilter(estates, freq = 4, type = "frequency")

# education
education_rf <- hpfilter(education_r, freq = 4, type = "frequency")
educationf <- hpfilter(education, freq = 4, type = "frequency")

# health
health_rf <- hpfilter(health_r, freq = 4, type = "frequency")
other_servicef <- hpfilter(other_service, freq = 4, type = "frequency")



# plotting cyclic components 
par(bty = "l", font = 1)
par((mfrow = c(1,2)))
plot(agri_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", lwd = 2)
plot(agrif$cycle, ylab = "", col = "red", main = "GDP - Agriculture", lwd = 2)

plot(mining_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", lwd = 2)
plot(miningf$cycle, ylab = "", col = "red", main = "GDP - Mining", lwd = 2)

plot(manufacturing_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(manufacturingf$cycle, ylab = "", col = "red", main = "GDP - Manufacturing", 
     lwd = 2)

plot(electricity_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(electricityf$cycle, ylab = "", col = "red", main = "GDP - Electricity", 
     lwd = 2)
plot(construction_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(constructionf$cycle, ylab = "", col = "red", main = "GDP - Construction", 
     lwd = 2)
plot(trade_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(tradef$cycle, ylab = "", col = "red", main = "GDP - Trade", 
     lwd = 2)
plot(transport_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(transportf$cycle, ylab = "", col = "red", main = "GDP - Transport", 
     lwd = 2)
plot(hotels_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(hotelsf$cycle, ylab = "", col = "red", main = "GDP - Hotels", 
     lwd = 2)
plot(financial_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(financialf$cycle, ylab = "", col = "red", main = "GDP - Financial", 
     lwd = 2)
plot(estates_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(estatesf$cycle, ylab = "", col = "red", main = "GDP - Estates", 
     lwd = 2)
plot(education_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(educationf$cycle, ylab = "", col = "red", main = "GDP - Education", 
     lwd = 2)
plot(health_rf$cycle, ylab = "", col = "blue", main = "Tax Revenue", 
     lwd = 2)
plot(other_servicef$cycle, ylab = "", col = "red", main = "GDP - Health", 
     lwd = 2)



tim <- 1:length(agri)
# computation of gdp growth by sector
lsagr <- lm(agri ~ tim)
lsmin <- lm(mining ~ tim)
lsman <- lm(manufacturing ~ tim)
lselec <- lm(electricity ~ tim)
lselec <- lm(electricity ~ tim)
lscons <- lm(construction ~ tim)
lstrad <- lm(trade ~ tim)
lstrans <- lm(transport ~ tim)
lshot <- lm(hotels ~ tim)
lsfin <- lm(financial ~ tim)
lsest <- lm(estates ~ tim)
lsedu <- lm(education ~ tim)
# lshealth_r <- lm(health_r ~ tim)
summary(lsagr)
summary(lsmin)
summary(lsman)
summary(lselec)
summary(lscons)
summary(lstrad)
summary(lstrans)
summary(lshot)
summary(lsfin)
summary(lsest)
summary(lsedu)
# computation of revenue growth by sector
lsagr_r <- lm(agri_r ~ tim)
lsmin_r <- lm(mining_r ~ tim)
lsman_r <- lm(manufacturing_r ~ tim)
lselec_r <- lm(electricity_r ~ tim)
lselec_r <- lm(electricity_r ~ tim)
lscons_r <- lm(construction_r ~ tim)
lstrad_r <- lm(trade_r ~ tim)
lstrans_r <- lm(transport_r ~ tim)
lshot_r <- lm(hotels_r ~ tim)
lsfin_r <- lm(financial_r ~ tim)
lsest_r <- lm(estates_r ~ tim)
lsedu_r <- lm(education_r ~ tim)
lshealth_r <- lm(health_r ~ tim)
summary(lsagr_r)
summary(lsmin_r)
summary(lsman_r)
summary(lselec_r)
summary(lscons_r)
summary(lstrad_r)
summary(lstrans_r)
summary(lshot_r)
summary(lsfin_r)
summary(lsest_r)
summary(lsedu_r)
summary(lshealth_r)

# estimation of elasticities
agrils <- lm(agri_r ~ agri)
minls <- lm(mining_r ~ mining)
manls <- lm(manufacturing_r ~ manufacturing)
elels <- lm(electricity_r ~ electricity)
conls <- lm(construction_r ~ construction)
tradls <- lm(trade_r ~ trade)
transls <- lm(transport_r ~ transport)
hotls <- lm(hotels_r ~ hotels)
finls <- lm(financial_r ~ financial)
estls <- lm(estates_r ~ estates)
eduls <- lm(education_r ~ education)
heals <- lm(health_r ~ other_service)

summary(agrils)
summary(minls)
summary(manls)
summary(elels)
summary(conls)
summary(tradls)
summary(transls)
summary(hotls)
summary(finls)
summary(estls)
summary(eduls)
summary(heals)

# Computation volatility measure of cyclic components
sd(agri_rf$cycle)
sd(agrif$cycle)
sd(mining_rf$cycle)
sd(miningf$cycle)
sd(manufacturing_rf$cycle)
sd(manufacturingf$cycle)
sd(electricity_rf$cycle)
sd(electricityf$cycle)
sd(construction_rf$cycle)
sd(constructionf$cycle)
sd(trade_rf$cycle)
sd(tradef$cycle)
sd(transport_rf$cycle)
sd(transportf$cycle)
sd(hotels_rf$cycle)
sd(hotelsf$cycle)
sd(financial_rf$cycle)
sd(financialf$cycle)
sd(financial_rf$cycle)
sd(financialf$cycle)
sd(estates_rf$cycle)
sd(estatesf$cycle)
sd(health_rf$cycle)
sd(other_servicef$cycle)

# Computation of correlation coefficient between cyclic components
cor.test(agri_rf$cycle, agrif$cycle, method = "kendall")
cor.test(mining_rf$cycle, miningf$cycle, method = "kendall")
cor.test(manufacturing_rf$cycle, manufacturingf$cycle, method = "kendall")
cor.test(electricity_rf$cycle, electricityf$cycle, method = "kendall")
cor.test(construction_rf$cycle, constructionf$cycle, method = "kendall")
cor.test(trade_rf$cycle, tradef$cycle, method = "kendall")
cor.test(transport_rf$cycle, transportf$cycle, method = "kendall")
cor.test(hotels_rf$cycle, hotelsf$cycle, method = "kendall")
cor.test(financial_rf$cycle, financialf$cycle, method = "kendall")
cor.test(estates_rf$cycle, estatesf$cycle, method = "kendall")
cor.test(education_rf$cycle, educationf$cycle, method = "kendall")
cor.test(health_rf$cycle, other_servicef$cycle, method = "kendall")
