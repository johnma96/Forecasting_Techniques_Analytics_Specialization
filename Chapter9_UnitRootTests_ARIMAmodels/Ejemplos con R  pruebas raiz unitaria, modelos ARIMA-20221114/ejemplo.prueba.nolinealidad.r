# pruebas de no linealidad


E = read.table("usd.bpound.txt", header = TRUE, stringsAsFactors = FALSE)
attach(E)

y = ts(plibra, frequency = 12, start = c(1971,01))

#-------------- validacion cruzada
T = length(y)
m = 24

yf = ts(y[(T-m+1):T], frequency = 12, start = c(1996,10))
yi = ts(y[1:(T-m)], frequency = 12, start = c(1971,01))


#-------------------------

library(forecast)

auto.arima(yi)
ARIMA(0,1,1)(0,0,1)[12] 
mod2 = stats::arima(yi,order=c(0,1,1),
seasonal=list(order=c(0,0,1),period=12))
require(lmtest)
coeftest(mod2)

#--------residuos
at=ts(mod2$residuals,frequency=12)
# at = y - fitted(mod2)
#--------grafica
ts.plot(at)

#--------pruebas de no linealidad
require(tseries)

terasvirta.test(at)
white.test(at)

#-----------------------------

n= 300
x=double(n)
et = rnorm(n,0,2)

## Generar serie no lineal
x[1] <- 2.0
x[2] = x[1]
x[3] = x[1]
for(t in (4:n)) {
x[t] <- 2.0+0.2*et[t-1]+0.5*et[t-1]*et[t-2] + 1.3*et[t-1]*et[t-3]+et[t]
}

x <- as.ts(x)
plot(x)
terasvirta.test(x)
white.test(x)


et <- as.ts(et)
terasvirta.test(et)
white.test(et)

