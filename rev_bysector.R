
rm(list = ls())
setwd("~/tra_business_cycle")
sink(file = "rev_bysector",append = FALSE)


rev_bysector <- read.csv("~/tra_business_cycle/rev_bysector.csv")
attach(rev_bysector)
agri <- ts(log(agri), frequency = 4, start = c(1998,1))
industry <- ts(log(Industry), frequency = 4, start = c(1998,1))
services <- ts(log(services), frequency = 4, start = c(1998,1))
households <- ts(log(households), frequency = 4, start = c(1998,1))

# ************
library(mFilter)
industry <- hpfilter(industry, freq = 4, type = "frequency")
plot(industry$trend,ylab = "", main = "Industry",lwd = 2, col = "blue")
lines(industry, lty = 1, lwd = 2, col = "red")
legend("topleft",legend = c("Trend","Actual"), lty = 1,
       col = c("blue", "red"), bty = "n", cex = 0.8, 
       text.col = c("blue", "red"))
plot(industry $cycle,ylab = "Year",main = "Cyclic component", col = "blue")
# plot(lttaxrevenuep$cycle,ylab = "",main = "Tax Revenue")
# ********

library(mFilter)
agri <- hpfilter(agri, freq = 4, type = "frequency")
plot(agri$trend,ylab = "", main = "Agriculture",lwd = 2, col = "blue")
lines(agri, lty = 1, lwd = 2, col = "red")
legend("topleft",legend = c("Trend","Actual"), lty = 1,
       col = c("blue", "red"), bty = "n", cex = 0.8, 
       text.col = c("blue", "red"))
plot(agri$cycle,ylab = "Year",main = "Cyclic component", col = "blue")
# plot(lttaxrevenuep$cycle,ylab = "",main = "Tax Revenue")
# ********

plot.ts(agri, ylim = c(0,18), ylab = "Logarithm", col = "black", 
        xaxp = c(1998,2014,16), bty = "l", lwd = 2, xlab = "Year")
lines(Industry, col = "blue", lwd = 2)
lines(services, col = "red", lwd = 2)
lines(households,col = "purple", lwd = 2)
legend("topleft", legend = c("Agriculture","Industry","Services","Households"),
       lty = 1, lwd = 2, col = c("black","blue","red", "purple"), bty = "n",
       cex = 0.8, ncol = 1, 
       text.col = c("black","blue","red", "purple"), inset = 0.01)

tim <- 1:68
ols_ag <- lm(agri~tim)
ols_ind <- lm(Industry~tim)
ols_serv <- lm(services~tim)
ols_hou <- lm(households~tim)

summary(ols_ag)
summary(ols_ind)
summary(ols_serv)
summary(ols_hou)

library(texreg)
texreg(
  list(ols_ag,ols_ind,ols_serv),
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,  single.row = FALSE,
  custom.model.names = c("Agriculture","Industry","Services"),
  custom.coef.names =c("Intercept","Time"),
  digits = 4,
  caption.above = TRUE,
  label = "tab:reg",
  caption = "Estimate of Growth Rates by OLS",
  float = "bh"
)
