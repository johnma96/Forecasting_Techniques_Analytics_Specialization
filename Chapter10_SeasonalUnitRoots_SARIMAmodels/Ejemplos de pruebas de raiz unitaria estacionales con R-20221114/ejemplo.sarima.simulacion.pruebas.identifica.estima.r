#------- Tres ejemplos con datos simulados de sarimas
#        aplicaciones de pruebas, identificacion , estimacion
#        pronosticos.

library(forecast)


install.packages("CombMSC_1.4.2.1.tar.gz", repos = NULL, type ="source")

library(CombMSC)


#---------------con CombMSC


#----------------------------1er Ejemplo

ti=0.8; ti4=-0.137; 
sigma= sqrt(2)

Y1 = sarima.Sim(n = 22, period =12, 
model=list(order=c(1,1,0),ar = ti,
ma = NULL, sd = sigma), 
seasonal=list(order=c(1,1,0),ar=ti4,ma = NULL))

ts.plot(Y1)

#-----eliminar la tendencia con stl()
m1 = stl(Y1, s.window = 'per')
s1 = m1$time.series[,1]; e1 = m1$time.series[,3];
y1 = s1+e1
y12 = Y1-m1$time.series[,2]

#-----eliminar la tendencia con Loess
t = seq(1,length(Y1))
mod1 = loess(Y1 ~ t, span=0.10,degree = 1,
control = loess.control(surface = "direct"))

yhat1 = mod1$fitted

y13 = Y1-yhat1

par(mfrow=c(2,2))
plot(t,Y1,type='l')
lines(t,yhat1,col='red')
lines(t,m1$time.series[,2],col='blue')
ts.plot(y1)
ts.plot(y12)
ts.plot(y13)

#-----implementacion de la prueba Canova-Hansen
require(uroot) 
res = ch.test(y1, 
type = "trigonometric",lag1 = TRUE, pvalue='raw') # lag1 = TRUE,
res

require(aTSA)
aTSA::adf.test(Y1,nlag=8)

#-----ocsb rechaza nula I(1,1) si tobs < tcrit
O = ocsb.test(Y1)
#------rechaza Ho siendo cierta P(rechaza H0 | H0 cierta)


auto.arima(Y1)

#------------------------------
dyos = diff(diff(Y1,12,1),1,1)
dys = diff(Y1,12,1)
dyo = diff(Y1,1,1)

auto.arima(dyos)


library(TSA)
res = armasubsets(dyos,nar=24, nma=24, y.name = "Y", ar.method = "ols")
plot(res)



# estimar
m1 = arima(Y1,order=c(1,1,2),
seasonal=list(order=c(0,1,1),period=12))
library(lmtest)
coeftest(m1)

# pronosticar
pr = predict(m1,n.ahead=24)$pred
 n = end(t)[1]
cbind(c(t,seq(n+1,n+24)),c(Y1,pr))

plot(c(t,seq(n+1,n+24)),c(Y1,pr),type='l')
lines(seq(n+1,n+24),pr,type='l',col='red',lwd=2)


#----------------------------2do Ejemplo

fi=-0.8; fi4=-0.37; ti=-0.64; ti4=-0.513; sigma= sqrt(0.014)

Y2 = sarima.Sim(n = 60, period =4, 
model=list(order=c(1,0,1),
ar = fi, ma = ti, sd = sigma), 
seasonal=list(order=c(1,1,1),ar=fi4,ma = ti4))

ts.plot(Y2)


#-----eliminar la tendencia con stl()
m2 = stl(Y2, s.window = 'per')
s2 = m2$time.series[,1]; e2 = m2$time.series[,3];
y2 = s2+e2
y22 = Y2-m2$time.series[,2]

#-----eliminar la tendencia con Loess
t = seq(1,length(Y2))
mod2 = loess(Y2 ~ t, span=0.25,
control = loess.control(surface = "direct"))
yhat2 = fitted(mod2)

y23 = Y2-yhat2

par(mfrow=c(2,2))
plot(t,Y2,type='l',col='gray')
lines(t,yhat2,col='red',lwd=2)
lines(t,m2$time.series[,2],col='blue',lwd=2)
ts.plot(y2)
ts.plot(y22)
ts.plot(y23)

#-----implementacion de la prueba Canova-Hansen

res = ch.test(y2, 
type = "trigonometric",lag1 = TRUE, pvalue='raw') # lag1 = TRUE,
res


#------------------------------

aTSA::adf.test(Y2,nlag=8)

#------------------------------
dyos = diff(diff(Y2,4,1),1,1)
dys = diff(Y2,4,1)
dyo = diff(Y2,1,1)

auto.arima(dyo)
auto.arima(dys)
auto.arima(Y2)

library(TSA)
res = armasubsets(dys,nar=14, nma=14, y.name = "Y", ar.method = "ols")
plot(res)


# estimar
m1 = arima(Y2,order=c(1,0,0),
seasonal=list(order=c(0,1,2),period=4))
library(lmtest)
coeftest(m1)

# pronosticar
pr = predict(m1,n.ahead=12)$pred
cbind(c(t,seq(241,252)),c(Y2,pr))

plot(c(t,seq(241,252)),c(Y2,pr),type='l')
lines(seq(241,252),pr,type='l',col='red',lwd=2)

#--------------------------------3er Ejemplo


ti=-0.377; ti4=-0.572; 
sigma= sqrt(0.0014)

Y3 = sarima.Sim(n = 12, period =12, 
model=list(order=c(0,1,1),ma = ti,
ar = NULL, sd = sigma), 
seasonal=list(order=c(0,1,1),ma=ti4,ar = NULL))

ts.plot(Y3)


#-----eliminar la tendencia con stl()
m3 = stl(Y3, s.window = 'per')
s3 = m3$time.series[,1]; e3 = m3$time.series[,3];
y3 = s3+e3
y32 = Y3-m3$time.series[,2]

#-----eliminar la tendencia con Loess
t = seq(1,length(Y3))
mod3 = loess(Y3 ~ t, span=0.75,degree = 2,
control = loess.control(surface = "direct"))

yhat3 = fitted(mod3)

y33 = Y3-yhat3

par(mfrow=c(2,2))
plot(t,Y3,type='l')
lines(t,yhat3,col='red')
lines(t,m3$time.series[,2],col='blue')
ts.plot(y3)
ts.plot(y32)
ts.plot(y33)

#-----implementacion de la prueba Canova-Hansen
require(uroot) 
res = ch.test(y3, 
type = "trigonometric",lag1 = TRUE, pvalue='raw') # lag1 = TRUE,
res

res = ch.test(y33, 
type = "trigonometric",lag1 = TRUE, pvalue='raw') # lag1 = TRUE,
res

require(aTSA)
aTSA::adf.test(Y3,nlag=8)

B=ocsb.test(Y3)
summary(B)
print(B)


#-----ocsb rechaza nula I(1,1)  tobs < tcrit
#------rechaza Ho siendo cierta P(rechaza H0 | H0 cierta)




auto.arima(Y3)

dyos = diff(diff(Y3,12,1),1,1)
library(TSA)
res = armasubsets(dyos,nar=24, nma=24, y.name = "Y", ar.method = "ols")
plot(res)


# estimar
m3 = arima(Y3,order=c(0,1,1),
seasonal=list(order=c(1,1,1),period=12))
library(lmtest)
coeftest(m3)

# pronosticar
pr = predict(m3,n.ahead=24)$pred
 n = end(t)[1]
cbind(c(t,seq(n+1,n+24)),c(Y3,pr))

plot(c(t,seq(n+1,n+24)),c(Y3,pr),type='l')
lines(seq(n+1,n+24),pr,type='l',col='red',lwd=2)






