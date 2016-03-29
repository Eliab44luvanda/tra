


setwd("~/tra_business_cycle")
rm(list = ls())
shares <- read.csv("shares.csv")
attach(shares)

agriculture <- ts(agriculture, start = 1998, end = 2013, frequency = 1)
industry <- ts(industry, start = 1998, end = 2013, frequency = 1)
services <- ts(services, start = 1998, end = 2013, frequency = 1)

par(font = 2)
plot.ts(agriculture, ylim = c(10,65), ylab = "Share (%)", col = "black", 
      xaxp = c(1998,2014,8), bty = "l", lwd = 2, xlab = "Year")
lines(industry, col = "blue", lwd = 2)
lines(services, col = "red", lwd = 2)
legend("topleft", legend = c("Agriculture","Industry","Services"),
       lty = 1, lwd = 2, col = c("black","blue","red"), bty = "n",
       cex = 0.8, ncol = 1, 
       text.col = c("black","blue","red"), inset = 0.01)

