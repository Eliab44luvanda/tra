

setwd("~/tra_business_cycle")
rm(list = ls())
rev_sec_shares <- read.csv("rev_sec_shares.csv")
attach(rev_sec_shares)

agriculture <- ts(agriculture, frequency = 4,  start = c(1998,1))
industry <- ts(industry, frequency = 4,  start = c(1998,1))
services <- ts(services, frequency = 4,  start = c(1998,1))
households <- ts(households, frequency = 4,  start = c(1998,1))


plot.ts(agriculture, ylim = c(0,100), ylab = "Share (%)", col = "black", 
        xaxp = c(1998,2014,16), bty = "l", lwd = 2, xlab = "Year")
lines(industry, col = "blue", lwd = 2)
lines(services, col = "red", lwd = 2)
lines(households, col = "orange", lwd = 2)
legend("topleft", legend = c("Agriculture","Industry","Services","Households"),
       lty = 1, lwd = 2, col = c("black","blue","red","orange"), bty = "n",
       cex = 0.8, ncol = 1, 
       text.col = c("black","blue","red","orange"), inset = 0.01)
