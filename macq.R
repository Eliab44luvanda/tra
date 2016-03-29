
rm(list = ls())
setwd("~/tra_business_cycle")
macq <- read.csv("macq.csv")
attach(macq)

agri <- log(agri)

agri <- ts(agri,start = 2005,end = 2014,frequency = 4)
mining <- ts(log(mining),start = 2005,end = 2014,frequency = 4)
manufac <- ts(log(manufac),start = 2005,end = 2014,frequency = 4)

plot.ts(agri)

require(mFilter)
agrif <- hpfilter(agri,freq = 16,type = "frequency")
plot(agrif$cycle,ylab = "")
plot(agrif$trend,ylab = "")

manf <- hpfilter(manufac,freq = 16,type = "frequency")
plot(manf$cycle,ylab = "")
plot(manf$trend,ylab = "")

