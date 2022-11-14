# analisis sarima de la serie de pasajeros (AirCanada)

library(xtable)
library(FitAR)

rm(list=ls())       
graphics.off()

data(AirPassengers)

# AirPassengers :  Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...
y=AirPassengers
yi = window(y,start=c(1949,1),end=c(1959,12))
yf = window(y,start=c(1960,1),end=c(1960,12))

#########################################
 
# examinar la serie 
   
#----modelo con tendencia lineal y estacionalidad
#----con variables indicadoras estacionales

library(forecast)

t = seq(1,length(yi))
It = seasonaldummy(yi)

mod1 = lm(yi ~ t + It)
summary(mod1)

e = mod1$residuals

library(TSA)

par(mfrow=c(3,2))
plot(t,e,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(e),xlab='x',main= '')
acf(e,30,ci.type="ma",main="")
pacf(e,30,main="")
qqnorm(e)
qqline(e,col=2)

Box.test(x = e, lag = 26, type="Ljung-Box")

#-------------analisis SARMA de residuos estructurales

e = ts(e,frequency=12)
auto.arima(e)
ARIMA(1,0,0)(1,0,0)[12] with zero mean 

mod2 = Arima(e,order=c(1,0,0),seasonal=
list(order=c(1,0,0),period=12))

library(lmtest)

coeftest(mod2)


e = mod2$residuals


# pronosticos 12 meses

nt = length(yf)
T = length(yi)
Itp = seasonaldummy(yi,nt)
tp = seq(T+1,T+nt,1)
 
prons.vi = predict(mod1,
data.frame(t = tp, It=I(Itp)))

prons.sar = predict(mod2,n.ahead=nt)$pred


prons.vi = ts(prons.vi,frequency=12,
start=c(1960,1),end=c(1960,12))



par(mfrow=c(1,1))
tf = seq(1,length(yf),1)
plot(tf,yf,type = 'o',ylim=c(min(yf),max(yf)))
lines(tf,prons.vi, type = 'b', pch = 2,col='red' )
lines(tf,prons.tot, type = 'b', pch = 3,col='blue' )
legend("topleft", 
c("obs","estructurales","estr+sarma"), 
col = c("black","red","blue"),      
pch = c(1,2,3))


A=rbind(
accuracy(yf,prons.vi), 
accuracy(yf,prons.tot))

rownames(A) = c("Struc","Struc+SARMA")
A

#########################################
# analisis con sarima
require(uroot)


hegy.out2 = hegy.test(x=y, 
deterministic = c(1,1,1),
pvalue = "raw")

hegy.out2

# hay evidencia de  raiz unitaria ordinaria y estacional

#############################################

# diferencia estacional: Y(t) - Y(t-12)  
yi.e <- diff(yi, 12, 1)
yi.d <- diff(yi,1,1)
yi.de <- diff(yi.e, 1, 1)

par(mfrow=c(2,2))
ts.plot(yi)
ts.plot(yi.e)
ts.plot(yi.d)
ts.plot(yi.de)


##### identificacion sarima
# 1) metodo con panel
res=armasubsets(y=yi.de,
nar=14,nma=14,
y.name='r',
ar.method='ols')

par(mfrow=c(1,1))

plot(res)

# ARIMA(1,1,2)(0,1,0)

# 2) usar auto.arima

auto.arima(yi)

# ARIMA(1,1,0)(0,1,0)[12] 
# detecta raiz unitaria ordinaria y estacional
# esta de acuerdo con prueba hegy
  

# recordar el modelo air-passegers

mod3 = arima(yi,order = c(1, 1, 0),
seasonal = list(
order = c(0, 1, 0), 
period = 12), 
include.mean=TRUE)

coeftest(mod3)

#xreg=t


e=mod3$residuals

par(mfrow=c(3,2))
plot(t,e,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(e),xlab='x',main= '')
acf(e,30,ci.type="ma",main="")
pacf(e,30,main="")
qqnorm(e)
qqline(e,col=2)

Box.test(x = e, lag = 26, type="Ljung-Box")


############################################

# pronosticos con sarima

m3.pred = predict(mod3,n.ahead = length(yf))
prons.sa = m3.pred$pred

par(mfrow=c(1,1))
tf = seq(1,length(yf),1)
plot(tf,yf,type = 'o',ylim=c(min(yf),max(yf)))
lines(tf,prons.vi, type = 'b', pch = 2,col='red' )
lines(tf,prons.tot, type = 'b', pch = 3,col='blue' )
lines(tf,prons.sa, type = 'b', pch = 4,col='magenta' )



legend("topleft", 
c("obs","estructurales","estr+sarma","sarima"), 
col = c("black","red","blue","magenta"),      
pch = c(1,2,3,4))


############################################

A=rbind(
accuracy(yf,prons.vi), 
accuracy(yf,prons.tot),
accuracy(yf,prons.sa))

rownames(A) = c("Struc","Struc+SARMA","SARIMA")
A


