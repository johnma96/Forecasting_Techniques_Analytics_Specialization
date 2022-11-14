#-Ejemplo de simulacion de ma(q)  
   
  
#----------------------colocar las raices en el plano

   
    library(polynom)

	r = complex(7)
    
	r[1] = 1.0140 - 1.0488i
	r[2] = Conj(r[1])
	r[3] = 2
    r[4] = 1.2 - 0.5i
    r[5] = Conj(r[4])
    r[6] = 1.123 - 0.085i
    r[7] = Conj(r[6])


    a = poly.calc(r)
    a = a/a[1]
    (Mod(polyroot(a)))


#----------------------


n = 1300
sigma = 0.0341
mu = 0.04

x = mu+arima.sim(n = n, list(ar = numeric(0), ma = a[-1]),
sd = sigma)

x = ts(x, frequency = 12)
    
#----------------------

require(TSA)

require(signal)

par(mfrow=c(2,2))

zplane(filt=rev(a),a=c(1))
ts.plot(x[1:180])
acf(x,15,ci.type='ma',drop.lag.0=TRUE)
pacf(x,15)

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
