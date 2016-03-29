
rm(list = ls())
setwd("~/tra_business_cycle")
sink(file = "elast",append = FALSE)

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

library(texreg)
texreg(
  list(agrils, minls, manls),
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,  single.row = FALSE,
  custom.model.names = c("Agriculture","Mining","Manufacturing"),
  custom.coef.names =c("Intercept","Agriculture GDP", "Mining GDP", 
                       "Manufacturing GDP"),
  digits = 4,
  caption.above = TRUE,
  label = "tab:reg",
  caption = "Estimate of Tax Elasticities by OLS",
  float = "bh"
)

texreg(
  list(elels, conls, transls),
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,  single.row = FALSE,
  custom.model.names = c("Electricity","Construction","Trade"),
  custom.coef.names =c("Intercept","Electricity GDP", "Construction GDP", 
                       "Trade GDP"),
  digits = 4,
  caption.above = TRUE,
  label = "tab:reg",
  caption = "Estimates of Tax Elasticities by OLS",
  float = "bh"
)


texreg(
  list(transls, hotls, finls),
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,  single.row = FALSE,
  custom.model.names = c("Transport","Hotels","Financial services"),
  custom.coef.names =c("Intercept","Transport GDP", "Hotels GDP", 
                       "Financial GDP"),
  digits = 4,
  caption.above = TRUE,
  label = "tab:reg",
  caption = "Estimates of Tax Elasticities by OLS",
  float = "bh"
)


texreg(
  list(estls, eduls, heals),
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,  single.row = FALSE,
  custom.model.names = c("Real estates","Education","Health"),
  custom.coef.names =c("Intercept","Real estates GDP", "Education GDP", 
                       "Health GDP"),
  digits = 4,
  caption.above = TRUE,
  label = "tab:reg",
  caption = "Estimates of Tax Elasticities by OLS",
  float = "bh"
)