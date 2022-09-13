
# Ejemplo con modelo local de tendencia amortiguado
G = read.table("series_histo_88_mpio_jun17.prn", 
header = TRUE, stringsAsFactors = FALSE)
attach(G)

# definir como serie de tiempo
y = ts(G$Totalviv,frequency=12,start=c(2009,1))
ts.plot(y)

# grafica con fechas en el eje x

np = length(y)

fecha = seq(as.Date("2009/01/01"), as.Date("2017/06/01"), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,y, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 2,col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#----- validacion cruzada


 m = 12
 n = length(y)
 yi = ts(y[1:(n-m)],frequency=12)
 yf = ts(y[(n-m+1):n],frequency=12)

 #---------------------------forecast
library(forecast)

mod7.m <- hw(yi, seasonal="add",damped=TRUE)
summary(mod7.m)

yhat.7 = mod7.m$model$fitted

(M = mod7.m$model$par[1:4])

str(mod7.m)

#--------------ajuste

plot(fecha[1:(n-m)],yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 2,col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha[1:(n-m)],yhat.7,col='blue')

source("medidas.yest.r")

(A=medidas.yest(yi,yhat.7,5))

#-----------pronosticos

ypron7 = forecast(mod7.m,h=m)$mean


#-------medidas de calidad de pronosticos

library(forecast)
ypron7 = ts(ypron7,frequency=12)
(H=accuracy(ypron7,yf))


tf = seq((n-m+1),n)

plot(tf,yf, type = 'o')
lines(tf,ypron7, type = 'b', pch = 5,col='red' )
legend("topleft", 
c("Obs",  "Max.ver-Holt-Winters"), 
pch = c(1, 3),
col = c("black","blue"))


 #---------------------------bayesforecast
 # stan_Hw: Fitting a Holt-Winters state-space model

library(bayesforecast)
mod6.m  = stan_ssm(yi,trend = TRUE,
seasonal = TRUE,damped = TRUE,
iter = 500,chains = 1)

sigma0 = mod6.m$stanfit@sim$samples[[1]]$sigma0

level = mod6.m$stanfit@sim$samples[[1]]$level

trend = mod6.m$stanfit@sim$samples[[1]][[3]]

damped = mod6.m$stanfit@sim$samples[[1]][[4]]


par(mfrow=c(2,2))
hist(sigma0,100)
points(mean(sigma0),0,pch=20,col='red',cex=2.5)

hist(level,100)
points(mean(level),0,pch=20,col='red',cex=2.5)

hist(trend,100)
points(mean(trend),0,pch=20,col='red',cex=2.5)

hist(damped,100)
points(mean(damped),0,pch=20,col='red',cex=2.5)


yhat.6 = fitted(mod6.m)

#----------ajuste
ti = seq(1,length(yi))
 
plot(fecha[1:(n-m)],yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 2,col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha[1:(n-m)],yhat.6,col='red')

(A=rbind(A,medidas.yest(yi,yhat.6,5)))

# -------pronosticos 

ypron6 = forecast(mod6.m,h=m)$mean

par(mfrow=c(1,1))

plot(tf,yf, type = 'o')
lines(tf,ypron7, type = 'b', pch = 5,col='red' )
lines(tf,ypron6, type = 'b', pch = 5,col='blue' )
legend("topleft", 
c("Obs", "Holt-Winters","Bayes Holt-Winters"), 
pch = c(1, 3, 5),
col = c("black","red","blue"))

#-------medidas de calidad de pronosticos

library(forecast)
ypron6 = ts(ypron6,frequency=12)
(H=rbind(H,accuracy(ypron6,yf)))


