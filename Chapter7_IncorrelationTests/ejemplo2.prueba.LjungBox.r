# Ejemplo 2.  prueba Ljung-Box
#-------------ejemplo accion Siemens bolsa Frankfurt
# https://es-us.finanzas.yahoo.com/q/hp?s=SIE.F+Precios+históricos

D = read.csv("siemens.SIE.F.csv", header=TRUE, stringsAsFactors=FALSE)
Xn = D$Close
Zn = diff(log(Xn),1,1)

#-------------graficar autocorrelacion (fac)

par(mfrow=c(2,2))
ts.plot(Xn)
acf(Xn,130,ci.type="ma")
ts.plot(Zn)
acf(Zn,10,ci.type="ma")


#-------------pruebas Ljung-Box
Box.test(Zn, lag = 15 , type =  "Ljung-Box")
Box.test(Zn, lag = 30 , type =  "Ljung-Box")
Box.test(Zn, lag = 90 , type =  "Ljung-Box")
Box.test(Zn, lag = 180 , type =  "Ljung-Box")


#-------------graficar autocorrelacion sin cero

par(mfrow=c(1,1))

acf(Zn,90,ci.type="ma")

B=acf(Zn,90,ci.type="ma")
plot(B$lag[-1],B$acf[-1],type='h',lwd=2)
abline(h=-2/sqrt(length(Zn)),col='blue',lty=2)
abline(h=2/sqrt(length(Zn)),col='blue',lty=2)

N = length(Zn)
k = 41
(rok = cor(Zn[1:(N-k+1)],Zn[k:N]))





