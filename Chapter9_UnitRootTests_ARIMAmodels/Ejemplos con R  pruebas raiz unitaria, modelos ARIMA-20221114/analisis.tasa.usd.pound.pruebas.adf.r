
# Ejemplo de pruebas de raiz unitaria adf

E = read.table("usd.bpound.txt", header = TRUE, stringsAsFactors = FALSE)
attach(E)

y = ts(plibra, frequency = 12, start = c(1971,01))

dy = diff(y,1,1)


require(TSA)
par(mfrow=c(1,3))

ts.plot(y)
acf(y,60,ci.type="ma",drop.lag.0=TRUE)

pacf(y)

ts.plot(dy)
acf(dy,30,ci.type="ma",drop.lag.0=TRUE)
pacf(dy)


#----------------------------- prueba DF, KPSS, PP

library(forecast)
ndiffs(y, alpha=0.05, test=c("kpss"), type = "trend")
ndiffs(y, alpha=0.05, test=c("adf"), type = "trend")
ndiffs(y, alpha=0.05, test=c("pp"), type = "trend")


#----------------------------- con libreria fUnitRoots

library(fUnitRoots)

( adfTest(y, lags = 1, type = "nc"))

( adfTest(y, lags = 2, type = "nc"))

( adfTest(y, lags = 1, type = "c"))

( adfTest(y, lags = 2, type = "c"))

( adfTest(y, lags = 1, type = "ct"))

( adfTest(y, lags = 2, type = "ct"))

#----------------------------- 
require(aTSA)
require(tseries)

tseries::adf.test(y)

aTSA::adf.test(y)


