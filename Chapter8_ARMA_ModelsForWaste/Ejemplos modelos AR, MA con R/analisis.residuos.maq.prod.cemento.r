#---------analisis de residuos MA(q)
library(car)

library(lmtest)
library(xtable)


archivo = "cementq.dat"
E = read.table(archivo, header = TRUE)
attach(E)
y = ts(y,frequency=4,start=c(1956,1),end=c(1994,3))
#----modelo con tendencia lineal y estacionalidad
#----con variables indicadoras estacionales
T = length(y)
t = seq(1,T)

library(forecast)
It = seasonaldummy(y)

mod1 = lm(y ~ t + It)
summary(mod1)
r = mod1$residuals

ys = fitted(mod1)

require(TSA)

#-----examinar los residuos
par(mfrow=c(2,2))
plot(t,r,type='o',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(r,60, drop.lag.0 = TRUE,ci.type="ma")
pacf(r,60,main="")
cpgram(r) # periodograma acumulado

#------- examinar residuos MA(6)
par(mfrow=c(2,1))
TSA::acf(r,60, drop.lag.0 = TRUE,ci.type="ma",main="")
pacf(r,60,main="")

#----------estimar un MA(q) para los residuos
mod4.1 = arma(r,order = c(0,6))
summary(mod4.1)
mod4.2 = arima(r,order = c(0,0,6))
summary(mod4.2)
AIC(mod4.2)
#-----
r4.2 = residuals(mod4.2)

#------- examinar residuos MA(6)
par(mfrow=c(2,2))
plot(t,r4.2,type='o',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(r4.2,60, drop.lag.0 = TRUE,ci.type="ma")
pacf(r4.2,60,main="")
cpgram(r4.2)


Box.test(r4.2, lag = 8, type = "Ljung-Box")
Box.test(r4.2, lag = 12, type = "Ljung-Box")

#------- pronosticos
m = 16
tp = seq(T+1,T+m)
Itp = seasonaldummy(y,m)

y.p = predict(mod1,data.frame(t=tp,It=I(Itp)))
r.p = predict(mod4.2,n.ahead=m)$pred
#---------sumar pronosticos estructurales y.p
#         con los ma(6) r.p
yr.p = y.p+r.p

m0 = 40
N = seq(T-m0,T)

par(mfrow=c(1,1))
plot(seq(T-m0,T),y[N],type='b',
ylim=c(1200,2200),xlim=c(T-m0,T+m),
ylab='produccion trim. tons.',
xlab='trimestre')
lines(seq(T+1,T+m),yr.p,lty=7,lwd=2,col='red')
lines(seq(T+1,T+m),y.p,lty=4,lwd=2,col='blue')

legend("topleft",
legend=c("observada","E+MA(6)","Estructural"),
col=c('black','red','blue'),lty=c(1,7,4))


#--------------------------
require(polynom)
theta = coef(mod4.2)
theta = c(1,theta[-7])
(Mod(polyroot(theta)))

require(signal)
zplane(filt=rev(theta),a=c(1))

source("armaRoots.r")
armaRoots(-theta[-1], n.plot = 400, digits = 4)



