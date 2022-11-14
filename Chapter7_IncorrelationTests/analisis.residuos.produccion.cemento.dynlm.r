# ejemplo de pruebas LB, DW y DWG
# para la serie de produccion de cemento
# ademas ejemplo con libreria dynlm


rm(list=ls())       
graphics.off()


archivo = "F:/curso pronósticos/Trabajos y datos/datos semestre 02 2011/cementq.dat"
archivo = "cementq.dat"


E = read.table(archivo, header = TRUE)
attach(E)
   
y = ts(y,frequency=4,start=c(1956,1),end=c(1994,3))

yi = window(y,start=c(1956,1),end=c(1992,3))
yf = window(y,start=c(1992,4),end=c(1994,3))


#----modelo con tendencia lineal y estacionalidad
#----con variables indicadoras estacionales

t = seq(1,length(yi))

library(forecast)

It = seasonaldummy(yi)

mod1 = lm(yi ~ t + It)
summary(mod1)
 
print(xtable(mod1),digits=6)

yhat1 = mod1$fitted.values

r = residuals(mod1)
#######################################


par(mfrow=c(2,2))
plot(t,r,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r),xlab='x',main= '')
acf(r,60,ci.type="ma",drop.lag.0=TRUE,main="")
#pacf(r,60,main="")
cpgram(r)


####################################################

# pruebas Ljung-Box 

Box.test(r, lag = 25, type = "Ljung-Box")
Box.test(r, lag = 35, type = "Ljung-Box")

# pruebas Durbin-Watson

library(lmtest)

dwtest(mod1) # de la libreria lmtest

library(car)
durbinWatsonTest(mod1,max.lag=5) # de la libreria car

####################################################

# modelo incorporando terminos rezagados de la serie
# utilizando la libreria dynlm

require(dynlm)

mod2 = dynlm(yi ~ t + It + L(yi,1)+ L(yi,2))
summary(mod2)

yhat2 = mod2$fitted.values
r2 = residuals(mod2)

par(mfrow=c(1,1))
# grafica residuos versus yhat
plot(as.numeric(yhat2),as.numeric(r2),type='p',pch=19)
abline(h=0,lty=2,lwd=2)

par(mfrow=c(2,2))
plot(t[-c(1,2)],r2,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r2),xlab='x',main= '')
acf(r2,60,ci.type="ma",drop.lag.0=TRUE,main="")
cpgram(r2)

Box.test(r2, lag = 25, type = "Ljung-Box")
Box.test(r2, lag = 35, type = "Box-Pierce")

dwtest(mod2)
durbinWatsonTest(mod2)

(B=durbinWatsonTest(mod2,max.lag=15))
B = data.frame(r=B$r,dw=B$dw,p=B$p)
library(xtable)
print(xtable(B,digits=4))



############################# ensayos adicionales

# ensayar un modelo exp cuadratico estacional
# incluyendo variables rezagadas de y


#---------modelo log cuadratico con estacionalidad
lyi = log(yi)
mod3 = dynlm(lyi ~ t + It + L(yi,1)+ L(yi,2))
summary(mod3)

coef0=coef(mod3) #extrae los coeficientes ajustados del modelo log lineal.

Xt = cbind(rep(1,T),t,It)
b1 = yi[2:(T-1)]
b2 = yi[1:(T-2)]
Xt1 = cbind(b1,b2)
Xt2 = Xt[-c(1:2),]
Xt = cbind(Xt2,Xt1)
colnames(Xt) = names(coef0)
yj = yi[3:T]
Ds = data.frame(yj,Xt)

mod4 = nls(yj~exp(Xt%*%theta),
data=Ds, start= list(theta=coef0))


summary(mod4)

yhat4 = fitted(mod4)
r = residuals(mod4)


par(mfrow=c(3,2))
plot(t[-c(1:2)],r,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r),xlab='x',main= '')
acf(r,60,ci.type="ma",main="")
pacf(r,60,main="")
qqnorm(r)
qqline(r,col=2)
# grafica residuos 
cpgram(r)

Box.test(r, lag = 15, type = "Ljung-Box")
Box.test(r, lag = 25, type = "Box-Pierce")

# las pruebas DW y DWG no estan diseñadas
# para residuos de regresion no lineal !!!
# no funcionan las pruebas siguientes...

durbinWatsonTest(mod4)

durbinWatsonTest(mod4,max.lag=10)



# pronosticos con el modelo lineal + estacionales
T = length(yi)
Itp = seasonaldummyf(yi,8)
tp = seq(T+1,T+8,1)
prons.vi = predict(mod1,
data.frame(t = tp, It=I(Itp)))

# pronosticos con el modelo con rezagos
# pero son pronosticos a un paso
# porque en cada tiempo se usa informacion
# del tiempo anterior
n = length(y)
lyf1=y[(n-8):(n-1)]
lyf2=y[(n-9):(n-2)]
Xt = cbind(rep(1,8),tp,Itp,lyf1,lyf2)
beta = mod2$coefficient
prons.din = Xt%*%beta

# grafica de pronosticos
plot(tp,yf, type = 'o', 
ylim = c(1400,2000))
lines(tp,prons.vi, type = 'b', pch = 5,col='red' )
lines(tp,prons.din, type = 'b', pch = 3,col='blue' )

(cbind(accuracy(yf,prons.vi),
accuracy(yf,prons.din)))



