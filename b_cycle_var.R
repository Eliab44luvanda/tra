
rm(list = ls())
setwd("~/tra_business_cycle")
sink(file = "b_cycle_var",append = FALSE)

# Opening data file
b_cycle_data <- read.csv("b_cycle_data.csv")
# View(b_cycle_data)
attach(b_cycle_data)

# Taking logs of variables
lgdp <- log(gdp2001)
lrexp <- log(rexp)
ldexp <- log(dexp)
ltexp <- log((rexp + dexp))
lm1 <- log(m1)
lm2 <- log(m2)
lm3 <- log(m3)
leduties <- log(eduties)
lincome <- log(income)
lothers <- log(others)
lttaxrevenue <- log(ttaxrevenue)
lexrate <- log(exrate)

# Time series variables
lgdp <- ts(lgdp, start = 1966, end = 2010, frequency = 1)
lngdp <- ts(log(gdp), start = 1966, end = 2010, frequency = 1)
lrexp <- ts(lrexp, start = 1966, end = 2010, frequency = 1)
ldexp <- ts(ldexp, start = 1966, end = 2010, frequency = 1)
ltexp <- ts(ltexp, start = 1966, end = 2010, frequency = 1)
lm2 <- ts(lm2, start = 1966, end = 2010, frequency = 1)
leduties <- ts(leduties, start = 1966, end = 2010, frequency = 1)
lincome <- ts(lincome, start = 1966, end = 2010, frequency = 1)
lothers <- ts(lothers, start = 1966, end = 2010, frequency = 1)
lttaxrevenue <- ts(lttaxrevenue, start = 1966, end = 2010, frequency = 1)
inflation <- ts(inflation, start = 1966, end = 2010, frequency = 1)
exrate <- ts(exrate, start = 1966, end = 2010, frequency = 1)

library(vars)
library(urca)

y <- cbind(lngdp, lm2)

VARselect(y,lag.max = 3, type = "both")
varmodel <- VAR(y, p = 1, type = "both", ic = "SC")
summary(varmodel)
causality(varmodel, cause = "lngdp", vcovHC(varmodel))
causality(varmodel, cause = "lm2", vcovHC(varmodel))
fevd(varmodel, n.ahead = 20)

x <- cbind(lttaxrevenue, lngdp)
z <- cbind(lincome, lngdp)
catax <- ca.po(x, type = "Pz")
summary(catax)

par(mfrow = c(1,2))
plot.ts(lngdp, ylim = c(4.5,17), ylab = "Logarithm", col = "red", bty = "l", 
lwd = 2)
lines(leduties, col = "blue", lwd = 2)
legend("topleft",legend = c("GDP","Import duty"), lty = 1,
       col = c("red", "blue"), bty = "n", cex = 0.8, 
       text.col = c("red", "blue"))


plot.ts(lngdp, ylim = c(4.5,17), ylab = "Logarithm", col = "red", bty = "l", 
        lwd = 2)
lines(lincome, col = "blue", lwd = 2)
legend("topleft",legend = c("GDP","Income Tax"), lty = 1,
       col = c("red", "blue"), bty = "n", cex = 0.8, 
       text.col = c("red", "blue"))

sj <- ca.jo(x, type = c("eigen", "trace"))
summary(sj)

sjs <- cajolst(x, trend = TRUE, K = 2)
summary(sjs)


zsj <- ca.jo(z, type = c("eigen", "trace"))
summary(zsj)

zsjs <- cajolst(z, trend = TRUE, K = 2)
summary(zsjs)
