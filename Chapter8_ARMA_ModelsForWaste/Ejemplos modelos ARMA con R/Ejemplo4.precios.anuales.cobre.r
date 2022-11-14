# Ejemplo 4, precios anuales  del cobre
# datos tomados de:

the annual copper prices from 1800 to 1997 
which can be downloaded in R via the tsdl library.
tsdl: The time series from the Time Series Data Library.

#-----------leer datos y graficar
require(tsdl)

y = as.numeric(
subset(tsdl, description = "copper")[[3]])

ts.plot(y)

#-----------eliminar tendencia con Loess
t = seq(1,length(y))
mod1 = loess(y ~ t, span=0.5,
control = loess.control(surface = "direct"))

yt = ts(y-fitted(mod1),start = 1800, 
freq = 1)

ts.plot(yt)

#----------examinar fac facp
library(astsa)
acf2(yt, max.lag = 20)

require(TSA)
par(mfrow=c(3,2))

ts.plot(yt)
plot(density(yt),xlab='x',main= '')
acf(yt,20,drop.lag.0=TRUE,main="")
pacf(yt,20,main="")
qqnorm(yt)
qqline(yt,col=2)

#-----------identificar
require(forecast)
auto.arima(yt)

#------------metodo de barrido, para escoger un posible
#            modelo ARMA(p,q),con p,q=0,1,2,3
nll = matrix(nrow=4,ncol=4,
dimnames=list(paste("p=",0:3,sep=""),
paste("q=",0:3,sep="")))
aic = nll
lb = nll
for (p in (0:3)) {
for (q in (0:3)) {
ARMAlabel = sprintf("ARMA(%d,%d)",p,q)
armamodel = arima(yt,order=c(p, 0, q),include.mean = TRUE)
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
p=3;q=2;

#-----------estimar

mod2 = stats::arima(yt,order = c(1, 0, 1), 
include.mean = FALSE)

require(lmtest)
coeftest(mod2)


mod3 = stats::arima(yt,order = c(3, 0, 2), 
include.mean = FALSE)


coeftest(mod3)

(c(AIC(mod2),AIC(mod3)))

#-----------validar

et = resid(mod3)
par(mfrow=c(3,2))

ts.plot(et)
plot(density(et),xlab='x',main= '')
acf(et,20,drop.lag.0=TRUE,main="")
pacf(et,20,main="")
qqnorm(et)
qqline(et,col=2)

Box.test(et, lag = 10,type="Ljung")

Box.test(et, lag = 20,type="Ljung")


#--------------  pronosticos
T = length(yt)
tf = seq(T+1,T+10)
pr.yw = predict(mod1, data.frame(t = tf))

py1 = pr.yw+predict(mod3, n.ahead=10)$pred
sde = predict(mod3, n.ahead=10)$se

b = c(y[(T-30+1):T],py1)
plot(1:40,b, type='b', col=2,ylim=c(-90,300))
points(31:40,py1, type='b', col='blue')
points(31:40, py1+1.64*sde, type='l', col='blue')
points(31:40, py1-1.64*sde, type='l', col='blue')


require(GeneralizedHyperbolic)


fit <- hyperbFit(et)
summary(fit)
param = c(-11.89919520,13.08114408,0.05075932,0.01190926)

# Cramer-von~Mises Test of a Hyperbolic Distribution

hyperbCvMTest(et,param=param)



d <- density(et)

plot(d$x,log(d$y), 
type = "l", col='red',lwd=1, 
xlab="x", 
ylab= "f(x)",main="")
lines(d$x,log(dhyperb(d$x,param=param)),
lwd=2,type = 'l', col='blue')



