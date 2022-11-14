
library(forecast)
library(lmtest)

library(timsac)
library(uroot)
library(urca)

rm(list=ls())       
graphics.off()



E = read.table("cementq.dat", header = TRUE)
attach(E)
   
y = ts(y,frequency=4,start=c(1956,1),end=c(1994,3))

T = length(y)

par(mfrow=c(2,2))
ts.plot(y)

ts.plot(diff(diff(y,4,1),1,1))

acf(diff(diff(y,4,1),1,1),
26, ci.type="ma")

pacf(diff(diff(y,4,1),1,1),26)

#----------------------------
require(aTSA)
A = aTSA::adf.test(y)
P = aTSA::pp.test(y, lag.short = TRUE, output = TRUE)

#----------------------------
CH = ch.test(y, type = "trigonometric", pvalue='raw')

H = hegy.test(y, deterministic = c(1,0,0))

O = ocsb.test(y)


