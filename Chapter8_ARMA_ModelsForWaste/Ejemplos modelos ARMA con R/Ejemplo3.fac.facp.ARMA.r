# Ejemplo 9.4.5 Notas de clase
# simulación, identificación, estimación y 
# predicción de un ARMA(3,2)
#----------raices autoregresivas 3
library(polynom)
z1 = complex(3)
z1[1] = -0.8 - 1.3i
z1[2] = Conj(z1[1])
z1[3] = 1.2
#---------polinomio AR
a = poly.calc(z1)
a = a/a[1]
source("armaRoots.r")
armaRoots(-a[-1], n.plot = 400, digits = 4)

#-----------raices media movil  2
z2 = complex(2)
z2[1] = -1.2 -0.5i
z2[2] = Conj(z2[1])
#-----------polinomio MA
b = poly.calc(z2)
(b = b/b[1])
armaRoots(-b[-1], n.plot = 400, digits = 4)

require(signal)
par(mfrow=c(1,1))
zplane(filt=rev(b),a=rev(a))

#------------- usar la funcion arima.sim 
#              con los coeficientes a y b.
n = 300
y = arima.sim(list(order=c(3,0,2), ar=-a[2:4],
ma=b[2:3]), n=n,sd=sqrt(0.3))
#--------------graficar
plot.ts(y)

#-------------- fac facp teórica

fac.k = ARMAacf(ar = -a[2:4],ma=b[2:3],lag.max = 20)

facp.k = ARMAacf(ar = -a[2:4],ma=b[2:3],lag.max = 20,pacf=TRUE)

tk = seq(1,20)
par(mfrow=c(2,2))
plot(tk,fac.k[-1],type='h',lwd=2,ylim=c(-0.1,1))
abline(h=0)
plot(tk,facp.k,type='h',lwd=2)
abline(h=0)
require(TSA)
acf(y,20,ci.type='ma',drop.lag.0=TRUE)
pacf(y,20)


#-------------- identificar el modelo

#------------metodo de barrido escoger un posible
#            modelo ARMA(p,q),con p,q=0,1,2,3
nll = matrix(nrow=4,ncol=4,
dimnames=list(paste("p=",0:3,sep=""),
paste("q=",0:3,sep="")))
aic = nll
lb = nll
for (p in (0:3)) {
for (q in (0:3)) {
ARMAlabel = sprintf("ARMA(%d,%d)",p,q)
armamodel = arima(y,order=c(p, 0, q),include.mean = TRUE)
lbtest = Box.test(na.omit(armamodel$resid), lag = 12,type="Ljung")
nll[p+1,q+1] = armamodel$loglik;
aic[p+1,q+1] = armamodel$aic;    
lb[p+1,q+1] = lbtest$p.value}} 
cat("LogLik:\n")
print(nll)
cat("AIC:\n")
print(aic)
cat("Box-Ljung-Test:\n")
print(lb)
min(aic)

p=3 ; q=2 ;




#-------------- para su estimacion se usa la instruccion
mod1 = arima(y, c(p,0,q))

#-------------- los pronosticos con este arma(2,3)
py1 = predict(mod1, n.ahead=20)
b = c(y[(300-90+1):300],py1$pred)
plot(1:110,b, type='b', col=2)
points(91:110,py1$pred, type='b', col='blue')
points(91:110, py1$pred+1.64*py1$se, type='l', col='blue')
points(91:110, py1$pred-1.64*py1$se, type='l', col='blue')
