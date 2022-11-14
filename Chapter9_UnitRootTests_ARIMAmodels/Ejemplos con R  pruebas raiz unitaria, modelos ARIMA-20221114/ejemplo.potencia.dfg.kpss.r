#  Ejemplos sobre potencia con DickeyFuller aumentada y KPSS

#-------------- simulacion de un ARMA(3,2)
#-------------- defina las raices del polinomio autorregresivo grado 3
library(forecast)
library(polynom)
# z1[1] = -0.8 - 0.62i 

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

source("armaRoots.r")

par(mfrow=c(2,2))
armaRoots(-b[-1],n.plot=400)
armaRoots(-a[-1],n.plot=400)

#------------- usar la funcion arima.sim con los coeficientes a y b.
n = 300
y0 = arima.sim(list(order=c(3,0,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))

ts.plot(y0)

y1 = arima.sim(list(order=c(3,1,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))

ts.plot(y1)

#---------H0: raiz unitaria

#         DF:    H0 cierta
y1 = arima.sim(list(order=c(3,1,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))

#         KPSS:  H0 cierta
y0 = arima.sim(list(order=c(3,0,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))

library(urca)
df.none = ur.df(y = y1, lags = 2, type = 'none' )
summary(df.none)

kpss.none = ur.kpss(y0, type="mu", lags="short")
summary(kpss.none)

#------------------------------
tau.df = double(1000)
tau.kpss = double(1000)
for(j in 1:1000){

y0 = arima.sim(list(order=c(3,0,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))
kpss.none = ur.kpss(y0, type="mu", lags="short")
tau.kpss[j] = kpss.none@teststat

y1 = arima.sim(list(order=c(3,1,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))
df.none = ur.df(y = y1, lags = 2, type = 'none' )
tau.df[j] = df.none@teststat}

df.crit = -1.95

kpss.crit = 0.463

hist(tau.df,100)
points(df.crit,0,pch=20,col='red',cex=2)

hist(tau.kpss,100)

points(kpss.crit,0,pch=20,col='red',cex=2)


#----------DF P( rechaza H0 | H0 cierta )
#----------KPSS P( rechaza H0 | H0 cierta )

(alfa = sum(tau.kpss > kpss.crit)/1000)
(alfa= sum(tau.df < df.crit)/1000)



#------------------------------
tau.df = double(1000)
tau.kpss = double(1000)
for(j in 1:1000){

y0 = arima.sim(list(order=c(3,0,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))
kpss.none = ur.kpss(y1, type="mu", lags="short")
tau.kpss[j] = kpss.none@teststat

y1 = arima.sim(list(order=c(3,1,2), ar=-a[2:4], ma=b[2:3]), n=n,
sd=sqrt(0.3))
df.none = ur.df(y = y0, lags = 2, type = 'none' )
tau.df[j] = df.none@teststat}


#----------KPSS P( rechaza H0 | H0 falsa )

(pot = sum(tau.kpss > kpss.crit)/1000)
(pot= sum(tau.df < df.crit)/1000)

