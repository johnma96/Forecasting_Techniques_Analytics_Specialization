#-------------- simulacion del modelo consumo energia
#               articulo de R. Weron
#-------------- defina las raices del polinomio autorregresivo grado 3
library(forecast)
library(polynom)
source("armaRoots.r")

#-------modelo ARMA(1,6)

a= c(1,-0.332776)

b =  c(1,-0.383245,-0.12908, -0.149307,0,0,-0.0531862)


# visualizar las raices de los polinomios 
# funcion armaRoots usa:  root = polyroot(c(1, -coefficients))
par(mfrow=c(2,2))
armaRoots(-a[2:2],n.plot=400)
armaRoots(-b[2:7],n.plot=400)

require(signal)
par(mfrow=c(1,1))
zplane(filt=rev(b),a=rev(a))

require(GeneralizedHyperbolic)
param <- c(
mu = 0.076975,
delta = 0.298285, 
alpha = 1.671304,
beta = -0.098790)

param1 <- c(
mu = 0.076975,
delta = 0.298285, 
alpha = 1.671304,
beta = -0.098790,
lambda = 1.9)

u= ghypMean(param=param1)
s = sqrt(ghypVar(param=param1))

n = 1720
et = rghyp(n,param=param1)

et.n = rnorm(n,mean=u,sd=s)


#------------- usar la funcion arima.sim
y = arima.sim(n = n, list(ar = -a[2:2], ma = b[2:7]),innov = et)
yn = arima.sim(n = n, list(ar = -a[2:2], ma = b[2:7]),innov = et.n)

ts.plot(cumsum(y),col='darkgray')
lines(cumsum(yn),col='orange')

#------------- 
require(TSA)
par(mfrow=c(2,2))
acf(y,30,ci.type='ma',drop.lag.0=TRUE)
acf(yn,30,ci.type='ma',drop.lag.0=TRUE)
pacf(y,30)
pacf(yn,30)


#-------------- identificar el modelo

#-------------- auto.arima identifica un arma(?,?)
y = ts(y,frequency=250)
auto.arima(y)
yn = ts(yn,frequency=250)
auto.arima(yn)


#-------------- para su estimacion se usa la instruccion
require(lmtest)
mod1 = arima(y, c(1,0,6))
summary(mod1)
(mod1$aic)
coeftest(mod1)

mod2 = arima(yn, c(1,0,6))
summary(mod2)
(mod2$aic)
coeftest(mod2)

require(robustarima)
mod3 <- arima.rob(y ~ 1, p=1,q=6, d=0)
summary(mod3)


M=cbind(c(-a[-1],b[-1]),mod1$coef[-8],mod2$coef[-8],
c(-mod3$model$ar,-mod3$model$ma))
colnames(M) = c("teoricos","ruido hyp",
"ruido Normal","est.rob")
(M)


r = mod1$residuals

rn = mod2$residuals


d <- density(r)

dn <- density(rn)

par(mfrow=c(2,2))
plot(d$x,d$y, 
type = "l", col='red',lwd=1, 
xlab="x", 
ylab= "f(x)",main="")
lines(dn$x,dn$y,col='blue')

plot(d$x,d$y, 
type = "l", col='red',lwd=1, 
xlab="x", 
ylab= "f(x)",main="")
lines(d$x,dghyp(d$x,param=param1),
lwd=2,lty=2, col='red')

plot(dn$x,dn$y, 
type = "l", col='blue',lwd=1, 
xlab="x", 
ylab= "f(x)",main="")
lines(d$x,dnorm(d$x,mean=u,sd=s),
lwd=2,lty=5, col='blue')


fit <- hyperbFit(r)
summary(fit)

param2 = c(0.0678,0.3004,1.6284,-0.0734,1)

lines(d$x,dghyp(d$x,param=param2),
lwd=2,lty=5, col='darkred')


#-------------- los pronosticos con  arma(3,2)

py = predict(mod1, n.ahead=10)

pyn = predict(mod2, n.ahead=10)

par(mfrow=c(1,1))

plot(1:50, c(y[681:720],py$pred), type='b', col=2)
abline(v=40)
lines(41:50, pyn$pred, type='b', col=1)



#------------- simular con la funcion filter.
x = stats::filter(et,b,"convolution",sides=1,circular=TRUE)
y = stats::filter(x,-a[2:2],"recursive",init=rep(0.076975,1))

xn = stats::filter(et.n,b,"convolution",sides=1,circular=TRUE)
yn = stats::filter(xn,-a[2:2],"recursive",init=rep(0.076975,1))

ts.plot(y,col='darkgray')
lines(yn,col='orange')


