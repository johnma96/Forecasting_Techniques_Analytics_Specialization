# analisis de dos series estacionales
# una simulada con sarima.sim
# otra simulada con un modelo componentes

library(forecast)
library(timsac)
library(TSA)
library(uroot)
library(urca)
library(fUnitRoots)
library(tseries)
library(lmtest)

rm(list=ls())       
graphics.off()


archivo2 = "serie.integrada.estacional.dat"


D = read.table(archivo2,
header = TRUE, stringsAsFactors = FALSE)
attach(D)

y = ts(y,frequency=12,start=c(1979,6))

#-------------------------------------

par(mfrow=c(1,1))
m1 = stl(y, s.window = 'per', t.window = 50, t.jump = 1)

s1 = m1$time.series[,1]
t1 = m1$time.series[,2]
e1 = m1$time.series[,3]

plot(m1)

require(trend)

res <- smk.test(y)
summary(res)

#-----implementacion de la prueba ADF
require(aTSA)
adf.test(y)

ndiffs(y)


#---------prueba Phillips-Perron para la serie USD/Pound
aTSA::pp.test(y, lag.short = TRUE, output = TRUE)


#-------------------------------------
# prueba canova-hansen
# primero se elimina la tendencia
# este procedimiento no esta en el articulo original de C.H.

y1 = s1+e1
# y1 = diff(y,1,1)  : esta es otra alternativa
ts.plot(y1)

# H0: estacional estacionaria
# Ha: raiz estacional, inestabilidad estructural

require(uroot) 
ch.out1 = ch.test(y1, 
type = "trigonometric", pvalue='raw')
ch.out1

nsdiffs(y1)

#-------------------------------------
## HEGY test with constant, 
# trend and seasonal dummies.

hegy.out2 = hegy.test(x=y1,
deterministic = c(1,0,0), 
lag.method = "fixed", 
maxlag = 1)

hegy.out2

#--------implementacion de la prueba ocsb
require(forecast)

# rechaza la nula de raiz unit est y ord si
# estadistico menor que valor critico

ocsb.test(y)
ocsb.test(y1)
Test statistic: 0.8007, 5% critical value: -1.803

# luego 0.8 > -1.803, luego no rechaza la nula

B = mat.or.vec(4,4)
B = matrix(rep(NA,16),4,4)
colnames(B) = c("I(0,0)","I(1,0)","I(0,1)","I(1,1)")
rownames(B)= c("ADF","CH","HEGY","OCSB")
B[1,2] = 1
B[2,3] =1
B[3,2] = 1
B[3,3] = 1
B[4,4] = 1

(B)


#-------------------------------------
# analizar un modelo estacionario estacional


It = seasonaldummy(y)
t = seq(1,length(y),1)
mod1  = lm(y ~ t + It )
summary(mod1)

yhat1 = mod1$fitted.values

r1 = mod1$residuals
r1 = ts(r1,frequency=12)


# analisis de los residuos  


par(mfrow=c(3,2))
plot(t,r1,type='l',ylab='residuo')
abline(h=0,lty=2)
plot(density(r1),xlab='x',main= '')
acf(r1,60,ci.type="ma",main="")
pacf(r1,60,main="")
qqnorm(r1)
qqline(r1,col=2)
cpgram(r1)

Box.test(r1,90)

auto.arima(r1)


#-------------------------------------
# analizar un modelo sarima: acepta Ho con CH
# toma diferencia estacional

# diferencia estacional: (1-L^(12))Y(t)=Y(t) - Y(t-12)  
y.d12 = diff(y, 12, 1)
y.d1 = diff(y, 1, 1)
y.d  = diff(y.d12,1,1)

par(mfrow=c(2,2))
ts.plot(y)
ts.plot(y.d12)
ts.plot(y.d1)
ts.plot(y.d)
#-------------------------------------

auto.arima(y)

auto.arima(y.d)
#-----------------
mod3 = arima(y,order=c(1,1,1),
seasonal=list(order=c(0,1,2),period=12))
library(lmtest)
coeftest(mod3)

r3 = mod3$residuals

t = seq(1,length(r3),1)

par(mfrow=c(3,2))
plot(t,r3,type='l',ylab='residuo')
abline(h=0,lty=2)
plot(density(r2),xlab='x',main= '')
acf(r3,60,ci.type="ma",main="")
pacf(r3,60,main="")
qqnorm(r3)
qqline(r3,col=2)
cpgram(r3)

Box.test(r3,90)

yhat3 = fitted(mod3)

#-------------------------------------

t = seq(1,length(y),1)
t3 = seq(1,length(yhat3))
par(mfrow=c(1,1))
plot(t,y,type='o', col='darkgray')
lines(t3,yhat3,col='magenta')
