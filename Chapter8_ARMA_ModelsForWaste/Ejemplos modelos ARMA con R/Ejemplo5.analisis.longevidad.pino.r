Ejemplo 5. longevidad de una especie de pino 
require(afmtools)

Centered annual pinus longaeva tree ring width measurement
time series 
width measurements at Mammoth Creek, Utah,
from 0 A.D to 1989 A.D.


data(MammothCreek)
y=MammothCreek-mean(MammothCreek)

y = ts(y,frequency=1)

ts.plot(y)

library(astsa)
acf2(y, max.lag = 40)

require(TSA)
par(mfrow=c(3,2))

ts.plot(y)
plot(density(y),xlab='x',main= '')
acf(y,60,main="")
pacf(y,60,main="")
qqnorm(y)
qqline(y,col=2)

#----------------identificacion
require(forecast)

auto.arima(y)
ARIMA(3,0,1) with zero mean 


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
p=2; q=2;

#------------estimacion
mod.arma31 = stats::arima(y,order=c(3,0,1))
summary(mod.arma31)
require(lmtest)
coeftest(mod.arma31)
et = na.omit(resid(mod.arma31))
AIC(mod.arma31)

mod.arma22 = stats::arima(y,order=c(2,0,2))
summary(mod.arma22)
coeftest(mod.arma22)
et = na.omit(resid(mod.arma22))
AIC(mod.arma22)

#-----------------validacion 

par(mfrow=c(2,2))

ts.plot(et)
plot(density(et),xlab='x',main= '')
acf(et,180,drop.lag.0=TRUE,main="")
pacf(et,90,main="")



Box.test(et, lag = 30,type="Ljung")

Box.test(et, lag = 90,type="Ljung")


#--------------  pronosticos
T = length(y)
tf = seq(T+1,T+10)
py22 = predict(mod.arma22, n.ahead=10)$pred
py31 = predict(mod.arma31, n.ahead=10)$pred

sde = predict(mod.arma22, n.ahead=10)$se

b = c(y[(T-30+1):T],py22)

par(mfrow=c(1,1))

plot(1:40,b, type='b', col=2)
points(31:40,py31, type='b', col='blue')
points(31:40, py22+1.64*sde, type='l', col='blue')
points(31:40, py22-1.64*sde, type='l', col='blue')


