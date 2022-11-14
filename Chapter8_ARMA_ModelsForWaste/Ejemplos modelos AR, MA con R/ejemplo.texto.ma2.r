#---------Simulación, Estimación y Pronósticos de un MA(2)

library(forecast,tseries)

n = 300
theta = c(1,-0.4,0.4)
(Mod(polyroot(theta)))

# Simulación: Función arima.sim

y = arima.sim(list(order=c(0,0,2), ma=theta[2:3]), n=n, sd=sqrt(2.3))

layout(1:3)
ts.plot(y)
acf(y,30)
pacf(y,30)

# Estimación: Función arma librería tseries

modelo.ma.1 = arma(x=y, order=c(0,2))
summary(modelo.ma.1) 

# Estimación: Función arima ( para pronosticar)

modelo.ma.2 = arima(x=y, order=c(0,0,2))
summary(modelo.ma.2) 

# Pronósticos

layout(1:2)

pred.y = predict(modelo.ma.2, n.ahead=3)
plot(seq(1,9), c(tail(y), pred.y$pred), type='b')
points(seq(7,9), pred.y$pred, type='b', col='red')


pred.y = predict(modelo.ma.2, n.ahead=13)
plot(seq(1,19), c(tail(y), pred.y$pred), type='b')
points(seq(7,19), pred.y$pred, type='b', col='red')

#--------------------------
 library(polynom)

    n = 3000
   
    t = seq(1,n,1)
	r = complex(7)
    
	r[1] = -1.0140 - 1.0488i
	r[2] = Conj(r[1])
	
    r[3] = -1.2 - 0.5i
    r[4] = Conj(r[3])
    r[5] = 1.123 - 0.205i
    r[6] = Conj(r[5])
    r[7] = -1.2

    a = poly.calc(r)
    a = a/a[1]
    (Mod(polyroot(a)))


sigma = 0.000341
     mu = 0.0004
     e = rnorm(n,0,sigma)
     x <- mu + na.omit(filter(e,  a, "conv",  sides=1, F))


	x = ts(x, frequency = 12)

    layout(1:3)
    ts.plot(x)
    acf(x,60)
    pacf(x,60)

    layout(1:1)

    require(signal)
    zplane(filt=as.vector(a),a=c(1))



