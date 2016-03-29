
# tidy_source(width.cutoff = 70)
setwd("~/tra_business_cycle")
sink(file = "business_cycle",append = FALSE)

# R Programme to estimate trend and cyclic components of 
# macroeconomic variables
# By Eliab G. Luvanda
# October 2015


rm(list = ls())
# Loading required packages
require(mFilter)

# Setting working dirctory

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

require(ggplot2)
ggplot(b_cycle_data,aes(year)) +
  geom_line(aes(y = lttaxrevenue), colour = "green") +
  geom_line(aes(y = lincome), colour = "blue") +
  geom_line(aes(y = leduties), colour = "red") +
  geom_line(aes(y = lothers), colour = "black") +
  scale_colour_manual("Direction", c("tax" = "green",
           "income" = "blue","duties" = "red", "others" = "black"))
 

# Time series variables
lgdp <- ts(lgdp, start = 1966, end = 2010, frequency = 1)
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

# b_cycle_data <- ts(b_cycle_data, start = 1966, end = 2010, frequency = 1)

# plot.ts(ttaxrevenue,type = "l",main = "Taxes",ylab = "")
# lines(income,lty = 2,lwd = 2)
# lines(eduties,lty = 3,lwd = 2)
# lines(others,lty = 4,lwd = 2)

plot.ts(lttaxrevenue,type = "l",main = "Taxes",ylab = "",ylim = c(0,18))
lines(lincome,lty = 2,lwd = 2)
lines(leduties,lty = 3,lwd = 2)
lines(lothers,lty = 4,lwd = 2)


 


# Plot variables on graph
par(bty = "l",font = 2)
plot.ts(lgdp,type = "l",main = "GDP",ylab = "")
plot.ts(lrexp,main = "Recurrent Expenditure",ylab = "")
plot.ts(ldexp,main = "Development Expenditure",ylab = "")
plot.ts(ltexp,main = "Government Expenditure",ylab = "")
plot.ts(lm2,main = "Money Supply",ylab = "")
plot.ts(leduties,main = "Import and Excise Duties",ylab = "")
plot.ts(lincome,main = "Income tax",ylab = "")
plot.ts(lothers,main = "Other Taxes",ylab = "")
plot.ts(lttaxrevenue,main = "Tax Revenue",ylab = "")
plot.ts(inflation,main = "Inflation",ylab = "")
plot.ts(exrate,main = "Exchange Rate",ylab = "TZS/USD")

# Estimation of trend and cyclic componemts by HP filter
# GDP
par(bty = "l",font = 1)
par(mfrow = c(1,2))

gdpf <- hpfilter(lgdp,type = "lambda")
plot(gdpf$trend,ylab = "", main = "GDP",lwd = 2, col = "blue")
lines(lgdp,lty = 1, lwd = 2, col = "red")
legend("topleft",legend = c("Trend","Actual"), lty = 1,
       col = c("blue", "red"), bty = "n", cex = 0.8, 
       text.col = c("blue", "red"))
plot(gdpf$cycle,ylab = "Year",main = "Cyclic component", col = "blue")
# plot(lttaxrevenuep$cycle,ylab = "",main = "Tax Revenue")


par(bty = "l", font = 1)
par((mfrow = c(1,2)))
# Tax revenue
 lttaxrevenuep <- hpfilter(lttaxrevenue,type = "lambda")
# plot(lttaxrevenuep$trend,ylab = "",main = "A: Tax Revenue",lwd = 2)
# lines(lttaxrevenue,lty = 2,lwd = 2)
# legend("topleft",c("Trend","Actual"),lty = 1:2,ncol = 2,bty = "n",cex = 0.7)
plot(lttaxrevenuep$cycle,ylab = "",main = "Tax Revenue", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

# Income tax
lincomep <- hpfilter(lincome, type = "lambda")
# plot(lincomep$trend,ylab = "", main = "A: Income Tax", lwd = 2)
# lines(lincome, lty = 2, lwd = 2)
# legend("topleft", c("Trend", "Actual"), lty = 1:2, ncol = 2, bty = "n", cex = 0.6)
plot(lincomep$cycle, ylab = "", main = "Income Tax", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

# Import and excise duties
ledutiesp <- hpfilter(leduties, type = "lambda")
# plot(ledutiesp$trend,ylab = "", main = "A: Import and Excise", lwd = 2)
# lines(leduties, lty = 2, lwd = 2)
# legend("topleft", c("Trend", "Actual"), lty = 1:2, ncol = 2, bty = "n", cex = 0.6)
plot(ledutiesp$cycle, ylab = "", main = "Import and Excise", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

# Other taxes
lothersp <- hpfilter(lothers, type = "lambda")
# plot(lothersp$trend,ylab = "", main = "A: Other Taxes", lwd = 2)
# lines(lothers, lty = 2, lwd = 2)
# legend("topleft", c("Trend", "Actual"), lty = 1:2, ncol = 2, bty = "n", cex = 0.6)
plot(lothersp$cycle, ylab = "", main = "Other Taxes", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

par(bty = "l",font = 1)
par(mfrow = c(1,2))
# Inflation
inflationp <- hpfilter(inflation,type = "lambda")
# plot(inflationp$trend,ylab = "", main = "A: Inflation",ylim = c(5,42),lwd = 2)
# lines(inflation,lty = 2,lwd = 2)
# legend("topright",c("Trend","Actual"),lty = 1:2,ncol = 2,bty = "n",cex = 0.6)
plot(inflationp$cycle, ylab = "", main = "Inflation", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

# Government Expenditure
ltexpp <- hpfilter(ltexp,type = "lambda")
# plot(ltexpp$trend,ylab = "", main = "A: Government Expenditure",lwd = 2)
# lines(ltexp,lty = 2,lwd = 2)
# legend("topleft",c("Trend","Actual"),lty = 1:2,ncol = 2,bty = "n",cex = 0.6)
plot(ltexpp$cycle,ylab = "",main = "Government Expenditure", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

# Money supply: m2
lm2p <- hpfilter(lm2,type = "lambda")
# plot(lm2p$trend,ylab = "", main = "Money Supply -M2",lwd = 2)
# lines(lm2,lty = 2,lwd = 2)
# legend("topleft",c("Trend","Actual"),lty = 1:2,ncol = 2,bty = "n",cex = 0.6)
plot(lm2p$cycle,ylab = "",main = "Money Supply -M2", col = "blue")
plot(gdpf$cycle,ylab = "",main = "GDP", col = "red")

# Unit root test: Zivot Andrews unit Root test
require(urca)
za.gdp <- ur.za(lgdp,model = "both", lag = 2)
summary(za.gdp)
za.ttaxrevenue <- ur.za(lttaxrevenue, model = "both",lag = 2)
summary(za.ttaxrevenue)
za.duties <- ur.za(leduties, model = "both", lag = 2)
summary(za.duties)
za.income <- ur.za(lincome, model = "both", lag = 2)
summary(za.income)
za.others <- ur.za(lothers, model = "both", lag = 2)
summary(za.others)

# Computation volatility measure of cyclic components
sd(gdpf$cycle)
sd(lttaxrevenuep$cycle)
sd(lincomep$cycle)
sd(ledutiesp$cycle)
sd(lothersp$cycle)
sd(inflationp$cycle)
sd(ltexpp$cycle)
sd(lm2p$cycle)

# Computation of correlation coefficient between cyclic components
cor.test(gdpf$cycle, lttaxrevenuep$cycle, method = "kendall",alternative = "g")
cor.test(gdpf$cycle, lincomep$cycle, method = "kendall")
cor.test(gdpf$cycle, ledutiesp$cycle, method = "kendall")
cor.test(gdpf$cycle, lothersp$cycle, method = "kendall")
cor.test(gdpf$cycle, inflationp$cycle, method = "kendall")
cor.test(gdpf$cycle, ltexpp$cycle, method = "kendall")
cor.test(gdpf$cycle, lm2p$cycle, method = "kendall")



require(MTS)
tax <- cbind(gdpf$cycle,lttaxrevenuep$cycle)
mq(tax,lag = 10)
inc <- cbind(gdpf$cycle, lincomep$cycle)
mq(inc,lag = 10)

duty <- cbind(gdpf$cycle, ledutiesp$cycle)
mq(duty,lag = 10)

othe <- cbind(gdpf$cycle,lothersp$cycle)
mq(othe,lag = 5)
