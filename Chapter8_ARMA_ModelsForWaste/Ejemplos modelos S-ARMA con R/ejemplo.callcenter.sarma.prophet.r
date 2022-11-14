
#-------------------Cargar los datos
datos = read.table("llamadas.banco.prn",
 header = TRUE, stringsAsFactors = FALSE)
attach(datos)

np = length(y)

fechas = as.Date(fecha,format="%d/%m/%y")

par(mfrow=c(2,1))

plot(fechas,y, xaxt="n",panel.first = grid(),type='l',ylab='')
axis.Date(1, at=seq(fechas[1],fechas[np], "months"), format="%m/%y")
axis.Date(1, at=seq(fechas[1],fechas[np], "years"), labels = FALSE, tcl = -0.2)

plot(fechas[1:70],y[1:70], xaxt="n",panel.first = grid(),type='b',ylab='')
axis.Date(1, at=seq(fechas[1],fechas[np], "months"), format="%m/%y")
axis.Date(1, at=seq(fechas[1],fechas[np], "years"), labels = FALSE, tcl = -0.2)


layout(1:2)
ts.plot(y)
ts.plot(log(y))

par(mfrow=c(3,2))

ts.plot(y)
plot(density(y),xlab='x',main= '')
acf(y,60,main="")
pacf(y,60,main="")
qqnorm(y)
qqline(y,col=2)


#-------------- validacion cruzada
T = length(y)
m = 14
yf = y[(T-m+1):T]
yi = ts(y[1:(T-m)],frequency=7)




#---------modelo de componentes serie prophet

library(prophet)

fecha.i = fechas[1:(T-m)]

history <- data.frame(ds = fecha.i,y = yi)
y.prophet <- prophet(history)

future <- make_future_dataframe(y.prophet, periods = 7,
freq="days", include_history = TRUE)

y.f <- predict(y.prophet, future)

yhat.prophet = y.f$yhat

np = length(yhat.prophet)

par(mfrow=c(1,1))
plot(fecha.i,yi,type='o',col='darkgray')
lines(fecha.i[1:np],yhat.prophet, col='magenta')

#---------residuos estructurales
yr = na.omit(yi[1:np] - yhat.prophet)
yr = ts(yr,frequency=7)
ti = seq(1,length(yr))

par(mfrow=c(3,2))
require(TSA)
plot(ti,yr,type='l',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(yr,60,ci.type="ma",drop.lag.0 = TRUE,main="")
pacf(yr,60,main="")
qqnorm(yr)
qqline(yr,col=2)
plot(density(yr),xlab='x',main= '')

cpgram(yr) # periodograma acumulado



par(mfrow=c(1,1))
plot(ti,yr,type='l',ylab='residuo')
abline(v=635)



