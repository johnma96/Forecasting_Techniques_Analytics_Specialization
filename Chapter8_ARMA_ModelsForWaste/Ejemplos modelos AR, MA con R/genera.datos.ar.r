#-Ejemplo de simulacion de ar(p)  
   
  
#----------------------colocar las raices en el plano
phi = c(1,-1.5,0.9)
polyroot(phi)


   
    library(polynom)
    r = complex(2)
	r[1] =  complex(real = 5/6, imaginary = sqrt(15)/6)
	r[2] = Conj(r[1])


    a = poly.calc(r)
    (a = a/a[1])
    (Mod(polyroot(a)))


#----------------------


n = 300
sigma = 0.0341
mu = 0.04

x = mu+arima.sim(n = n, list(ar =  -a[-1]),
sd = sigma)

x = ts(x, frequency = 12)
    
#----------------------

require(TSA)


par(mfrow=c(1,1))

source("armaRoots.r")
armaRoots(-a[-1])

require(forecast)


require(FitAR)
InvertibleQ(-a[-1])



#----------------------
The default  is to use conditional-sum-of-squares 
to find starting values, then maximum likelihood. 

   mod = arima(x, order=c(0,0,7))
   summary(mod)

(theta = mod$coef)
(cbind(a[-1],theta[-8]))

library(lmtest)
coeftest(mod)

#----------------------
 library(tseries)

   mod = arma(x, order=c(0,7))
   summary(mod)

#----------------------
Fits an ARIMA(p,d,q) model using the algorithm 
given in McLeod and Zhang (2007).

library(FitARMA)
out2<-FitARMA(x, c(0,0,7))
out2
coef(out2)


M=cbind(a[-1],theta[-8],-out2$thetaHat)
colnames(M) = c("teoricos","arima","FitARMA")
(M)


#----------------------
 e = rnorm(n,0,sigma)
y <- mu + na.omit(stats::filter(e,  
a[-1], "conv",  sides=1))

par(mfrow=c(3,2))
acf(x,15,ci.type='ma',drop.lag.0=TRUE)
pacf(x,15)
acf(y,15,ci.type='ma',drop.lag.0=TRUE)
pacf(y,15)
ts.plot(x[1:180])
ts.plot(y[1:180])
