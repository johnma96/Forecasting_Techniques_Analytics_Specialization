# Ejemplo de analisis AR(p)
# Serie de aportes hidrológicos diarios
# en Gw/h
# 

#-----------------datos
D = read.table("Precio.Aportes.prn", header = T, stringsAsFactors=FALSE)
attach(D)
 
Ap = ts(Ap,frequency = 360,start=c(1998,6))
ts.plot(Ap)

#-----------------modela estacionalidad con sen, cos
#---------------- serie con frecuencia diaria

require(forecast)

Ap = ts(Ap,frequency = 365)

It.trig = fourier(Ap,4)

t = seq(1,length(Ap))/100

mod2 = lm(Ap ~ t + It.trig)
summary(mod2)

r = ts(residuals(mod2),frequency=1)

#-----------------examen fac y fac parcial residuos

require(TSA)
par(mfrow=c(2,2))
ts.plot(r)
TSA::acf(r,180, drop.lag.0 = TRUE,ci.type="ma",main="fac")
pacf(r,60,main="fac parcial")

#-----------------identifica
#install.packages("FitAR")
require(FitAR)

n = length(Ap)
pvec = SelectModel(r, ARModel="AR", 
Criterion="BIC", 
lag.max=floor(10*log10(n)), Best=1)
(p=pvec)
mod.ar9 = stats::arima(r,order=c(9,0,0))
summary(mod.ar9)
require(lmtest)
coeftest(mod.ar9)
et = na.omit(resid(mod.ar9))
AIC(mod.ar9)
#-----------------
ar(r,aic = TRUE, method=c("burg"))

#----------------- 
mod.ar30 = stats::arima(r,order=c(30,0,0))
summary(mod.ar30)

coeftest(mod.ar30)
et = na.omit(resid(mod.ar30))
AIC(mod.ar30)
#----------------- 
auto.arima(r)
ARIMA(2,0,3) with zero mean
mod.arma32 = stats::arima(r, order = c(2, 0, 3))

coeftest(mod.arma32)
AIC(mod.arma32)
et = na.omit(resid(mod.arma32))
#----------------- 

par(mfrow=c(2,2))

ts.plot(et)
plot(density(et),xlab='x',main= '')
acf(et,180,drop.lag.0=TRUE,main="")
pacf(et,90,main="")

Box.test(et, lag = 30,type="Ljung")

Box.test(et, lag = 90,type="Ljung")


#----------------- la fac teorica
phi = mod.ar9$coef
fac.ar9= ARMAacf(ar=phi,ma=numeric(0),lag.max=180)

phi = mod.ar30$coef
fac.ar30= ARMAacf(ar=phi,ma=numeric(0),lag.max=180)

phi = mod.arma32$coef[1:2]
theta = mod.arma32$coef[3:5]
fac.arma32= ARMAacf(ar=phi,ma=theta,lag.max=180)

F=acf(r,180,drop.lag.0=TRUE, plot = FALSE)
par(mfrow=c(1,1))
h = seq(1,180)
plot(h,F$acf,type='h',col='darkgray')
points(h,fac.ar9[-1],pch=19,col='orange')
points(h,fac.ar30[-1],pch=19,col='red')
points(h,fac.arma32[-1],pch=19,col='blue')


#-----------------pronosticos

It.trig.p = fourier(Ap,4,30)
tp = seq(length(Ap)+1,length(Ap)+30)/100

yp.est = predict(mod2,data.frame(t=tp,It.trig=I(It.trig.p)))

yp.ar30 = predict(mod.ar30,n.ahead=30)$pred

se = predict(mod.ar30,n.ahead=30)$se

yp = yp.est+yp.ar30


par(mfrow=c(1,1))
T = length(Ap)

plot(seq(T-180,T+30), c(Ap[(T-180):T],yp), type='b', col=2)

points(seq(T+1,T+30),yp, type='b', col='blue')
points(seq(T+1,T+30), yp+1.64*se, type='l', col='blue')
points(seq(T+1,T+30), yp-1.64*se, type='l', col='blue')





