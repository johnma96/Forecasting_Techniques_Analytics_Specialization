#------------- Ejemplo la funcion arima.sim
# con los coeficientes a y b.
Phi3 = 1 - 0.1466381*x - 0.1430615*xˆ2 - 0.3576538*xˆ3
Theta2 = 1 + 1.420118*x + 0.591716*xˆ2

a = c(1, - 0.1466, - 0.1430, - 0.3576)
b = c(1, 1.4201, 0.5917)

y = arima.sim(list(order=c(3,0,2), ar=-a[2:4],
ma=b[2:3]), n = 300,sd=sqrt(0.3))

ts.plot(y)


#------------- usar la funcion filter.
et = rnorm(n= 300,0,sqrt(0.3))
x = stats::filter(et,b,"convolution",sides=1,circular=TRUE)
y1 = stats::filter(x,-a[2:4],"recursive",init=rep(0,3))

ts.plot(y1)