# Ejemplo coeficientes s-arma(1,2)(1,1)[12]

library(polynom)

a = as.polynomial(c(1,-0.3,rep(0,10),-0.12,0.12*0.3))
(a)

b = as.polynomial(c(1,0.07255651,- 0.6294622,rep(0,9),
- 0.0209915,0.001454504,0.01113709))
(b)

# visualizar las raices de los polinomios 
# funcion armaRoots usa:  root = polyroot(c(1, -coefficients))
source("armaRoots.r")
par(mfrow=c(2,2))
armaRoots(-b[-1],n.plot=400)
armaRoots(-a[-1],n.plot=400)

require(signal)
par(mfrow=c(1,1))
zplane(filt=rev(b),a=rev(a))
#----------------------------------
ar = polynomial(c(1,-0.3))
ma = polynomial(c(1,0.072,- 0.629))
ars = polynomial(c(1,-0.12))
mas = polynomial(c(1,- 0.021))

period = 12
bs = polynomial(c(rep(0,period),1))
ars = predict(ars,bs)  
mas = predict(mas,bs)

fullarpoly <- ar*ars
fullmapoly <- ma*mas

(Mod(polyroot(fullarpoly)))
(Mod(polyroot(fullmapoly)))

mo <- list()
mo$ar <- coef(fullarpoly)
mo$ma <- coef(fullmapoly)


require(signal)
par(mfrow=c(1,1))
zplane(filt=rev(mo$ma),a=rev(mo$ar))
#----------------------------------
n = 600
y = arima.sim(list(order=c(13,0,14), ar=-mo$ar[-1],
ma=mo$ma[-1]), n=n,sd=sqrt(0.3))

ts.plot(y)
y = ts(y,frequency=12)
#-----------verifica periodo

spectrum(y, spans=c(7), log="dB", ci=0.8)
abline(v=1)
spec.pgram(y,kernel("modified.daniell", c(3,3)), log="no")
abline(v=1)

require(timsac)
raspec(h = 100, var = 0.3^2, arcoef = -mo$ar[-1],
macoef = mo$ma[-1])
abline(v=1/12)

#-----------identifica
library(TSA)
res=armasubsets(y=y,nar=14,nma=14,y.name='y',
ar.method='ols')
plot(res)

require(forecast)
auto.arima(y)




