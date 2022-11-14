#----- Ejemplo Notas simular el modelo airline
ti=-0.377; ti12=-0.572; 
sigma= sqrt(0.0014)

library(CombMSC)

y = sarima.Sim(n = 20, period =12, 
model=list(order=c(0,1,1),ma = ti,
ar = NULL, sd = sigma), 
seasonal=list(order=c(0,1,1),ma=ti12,ar = NULL),
rand.Gen.Fun = rnorm, rand.Gen.Seas = rnorm)

Wt = diff(diff(y,12,1),1,1)
par(mfrow=c(2,2))
ts.plot(y)
ts.plot(Wt)
acf(Wt,48,drop.lag.0=TRUE,ci.type="ma")
pacf(Wt,48)

# identificar
auto.arima(dy)
ARIMA(0,0,1)(0,0,1)[12] with zero mean 

# identificador armasubset() libreria TSA
res=armasubsets(y=dy,
nar=14,nma=14,
y.name='dy',
ar.method='ols')
layout(1:1)
plot(res)

# estimar
m1 = arima(y,order=c(0,1,1),
seasonal=list(order=c(0,1,1),period=12))
summary(m1)

# pronosticar
pr = predict(m1,n.ahead=12)$pred