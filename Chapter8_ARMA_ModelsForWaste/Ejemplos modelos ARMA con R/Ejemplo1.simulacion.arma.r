#-------------- simulacion de un ARMA(3,2)
#-------------- defina las raices del polinomio autorregresivo grado 3
library(forecast)
library(polynom)
source("armaRoots.r")

z1 = complex(3)
z1[1] = -0.8 - 1.3i
z1[2] = Conj(z1[1])
z1[3] = 1.2
a = poly.calc(z1)
(a = a/a[1])
#-------------- defina las raices del polinomio de media movil de grado 2
z2 = complex(2)
z2[1] = -1.2 -0.5i
z2[2] = Conj(z2[1])
#-------------- los coeficientes estan en el vector b
b = poly.calc(z2)
b = b/b[1]
(b)

# visualizar las raices de los polinomios 
# funcion armaRoots usa:  root = polyroot(c(1, -coefficients))
par(mfrow=c(2,2))
armaRoots(-a[2:4],n.plot=400)
armaRoots(-b[2:3],n.plot=400)

#------------- usar la funcion arima.sim con los coeficientes a y b.
n = 3000
y = arima.sim(list(order=c(3,0,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))

#------------- comparar las series y, y1
library(TSA)
par(mfrow=c(2,2))
ts.plot(y,col='gray')
acf(y,20,ci.type='ma',drop.lag.0=TRUE)
pacf(y,20)

#-------------- identificar el modelo


nll = matrix(nrow=4,ncol=4,dimnames=list(paste("p=",0:3,sep=""),paste("q=",0:3,sep="")))
aic = nll
lb = nll
for (p in (0:3)) {
for (q in (0:3)) {
    ARMAlabel = sprintf("ARMA(%d,%d)",p,q)
    armamodel = arima(y,order=c(p, 0, q),include.mean = TRUE)
    lbtest = Box.test(na.omit(armamodel$resid), lag = 12,type="Ljung")
    nll[p+1,q+1] = armamodel$loglik;
    aic[p+1,q+1] = armamodel$aic;    
    lb[p+1,q+1] = lbtest$p.value
}
} 
cat("LogLik:\n")
print(nll)
cat("AIC:\n")
print(aic)
cat("Box-Ljung-Test:\n")
print(lb)

min(aic)


#-------------- auto.arima identifica un arma(?,?)
y = ts(y,frequency=12)
auto.arima(y,stationary=TRUE,seasonal=FALSE)

#--------------  estimacion 
mod1 = arima(y, c(3,0,2),include.mean = TRUE)
summary(mod1)
(mod1$aic)

require(lmtest)
coeftest(mod1)

require(FitARMA)
mod2 = FitARMA(y, order = c(3, 0, 2), demean = TRUE)
summary(mod2)


M=cbind(mod1$coef,c(mod2$phiHat,-mod2$thetaHat,mod2$muHat))
colnames(M) = c("arima","FitARMA")
(M)


#-----------validar

require(signal)
ae = c(1,-mod1$coef[1:3])
be = c(1,mod1$coef[4:5])
par(mfrow=c(1,1))
zplane(filt=rev(be),a=rev(ae))
#-----------validar

et = resid(mod1)
par(mfrow=c(3,2))

require(TSA)
ts.plot(et)
plot(density(et),xlab='x',main= '')
acf(et,20,drop.lag.0=TRUE,main="")
pacf(et,20,main="")
qqnorm(et)
qqline(et,col=2)

Box.test(et, lag = 10,type="Ljung")

Box.test(et, lag = 20,type="Ljung")




#-------------- los pronosticos con  arma(3,2)

py1 = predict(mod1, n.ahead=20)

par(mfrow=c(1,1))

plot(1:70, c(y[251:300],py1$pred), ylim=c(-3,4),
type='b', col='black')

points(51:70,py1$pred, type='b', col='blue')
points(51:70, py1$pred+1.64*py1$se, type='l', col='blue')
points(51:70, py1$pred-1.64*py1$se, type='l', col='blue')


#------------representacion MA(inf)

mod3 = arima(y, c(4,0,3),include.mean = TRUE)

psi.teo = ARMAtoMA(ar=-a[2:4], ma=b[2:3],10)
psi.est = ARMAtoMA(ar=mod1$coef[1:3],ma= mod1$coef[4:5], 10)
psi.est.2 = ARMAtoMA(ar=mod3$coef[1:4],ma= mod3$coef[5:7], 10)


(cbind(psi.teo,psi.est,psi.est.2))


