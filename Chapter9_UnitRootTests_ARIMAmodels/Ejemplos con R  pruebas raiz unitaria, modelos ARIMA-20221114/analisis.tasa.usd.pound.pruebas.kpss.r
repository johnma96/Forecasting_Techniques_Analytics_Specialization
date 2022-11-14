
# Ejemplo de pruebas de raiz unitaria
# pruebas de no linealidad
# combinacion de pronosticos
# pronosticos con modelo hibrido arima nnar

E = read.table("usd.bpound.txt", header = TRUE, stringsAsFactors = FALSE)
attach(E)

y = ts(plibra, frequency = 12, start = c(1971,01))

dy = diff(y,1,1)


require(TSA)
par(mfrow=c(2,2))

ts.plot(y)
acf(y,30,ci.type="ma",drop.lag.0=TRUE)
pacf(y)

ts.plot(dy)
acf(dy,30,ci.type="ma",drop.lag.0=TRUE)
pacf(dy)

#-----------------------------
Computes the Kwiatkowski-Phillips-Schmidt-Shin 
(KPSS) test for the null hypothesis that x is 
level or trend stationary.

To estimate sigma^2 the Newey-West estimator is used. 
If lshort is TRUE, then the truncation lag parameter 
is set to trunc(4*(n/100)^0.25), 
otherwise trunc(12*(n/100)^0.25) is used. 
The p-values are interpolated from Table 1 
of Kwiatkowski et al. (1992). If the computed statistic 
is outside the table of critical values, 
then a warning message is generated.

#-----------------------------

require(tseries)

kpss.test(y, null = "Trend")

kpss.test(y, null = "Level")

require(aTSA)

aTSA::kpss.test(y, lag.short = TRUE, output = TRUE)


aTSA::pp.test(y, lag.short = TRUE, output = TRUE)


#-------------- validacion cruzada
T = length(y)
m = 30

yf = ts(y[(T-m+1):T], frequency = 12, start = c(1996,10))
yi = ts(y[1:(T-m)], frequency = 12, start = c(1971,01))


#-------------------------

library(forecast)

require(forecastHybrid)

mod1 <- hybridModel(yi, weights="equal")

mod1$auto.arima
mod1$ets
mod1$stlm

B=forecast(mod1,h=m)
Bf = B$pointForecasts
colnames(Bf)
p1 = apply(Bf,1,mean)
plot(B)


T0 = 90
b = c(yi[(T-m-T0):(T-m)],yf)
T1 = length(yi)
tt = seq((T-m+1),T)

par(mfrow=c(1,1))

plot(seq((T-m-T0),T),b, 
type='l', col=2,ylim=c(0.5,3.0))
lines(tt,p1, col='blue')

lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","cuad+fourier+sarma"), 
lty = c(1,1),
lwd=c(2,2),
col=c('red','blue') )


#-------------------------
auto.arima(yi)
ARIMA(0,1,1)(0,0,1)[12] 
mod2 = stats::arima(yi,order=c(0,1,1),
seasonal=list(order=c(0,0,1),period=12))
require(lmtest)
coeftest(mod2)
#--------residuos
at=ts(mod2$residuals,frequency=12)
#--------pruebas de no linealidad
terasvirta.test(at)
white.test(at)
#--------grafica
ts.plot(at)
#--------ajuste NNAR
mod3=nnetar(at)
print(mod3)
#---------pronosticos
pr.nnar = forecast(mod3,h=m)$mean
pr.nnar = ts(pr.nnar,frequency=12,start=c(1996,10))
pr.arima = predict(mod2,m)$pred
sde = predict(mod2,m)$se
pr.arima = ts(pr.arima,frequency=12,start=c(1996,10))
pr = pr.nnar+pr.arima

par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='l', col=2,ylim=c(0.5,3.0))
lines(tt,pr, col='blue')
lines(tt,pr.arima, col='black')
lines(tt, pr+1.64*sde,  col='magenta')
lines(tt, pr-1.64*sde,  col='magenta')

legend("topright", 
c("obs","arima","arima+nnar"), 
lty = c(1,1,1),
lwd=c(2,2,2),
col=c('red','black','blue') )

#-------------------------
require(nonlinearTseries)
nonlinearityTest
#-------------------------

install.packages("nonlinearAnalysis", repos="http://R-Forge.R-project.org")

require(nonlinearAnalysis)

G=nonlinearityTest(y, verbose = TRUE)

G[[1]]
G[[2]]
G[[3]]
G[[4]]
G[[5]]
G[[6]]

#-----------------------------
require(tseries)
n= 300
x=double(n)

## Generate time series which is nonlinear in ``mean''
x[1] <- 0.0
for(i in (2:n)) {
  x[i] <- 0.4*x[i-1] + tanh(x[i-1]) + rnorm(1, sd=0.5)
}
x <- as.ts(x)
plot(x)
terasvirta.test(x)
white.test(x)


terasvirta.test(y)
white.test(y)

terasvirta.test(dy)









