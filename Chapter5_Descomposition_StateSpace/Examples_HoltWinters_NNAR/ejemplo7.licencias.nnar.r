
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

y.nnar = nnetar(yi,lambda=0)
print(y.nnar)

#----------valores ajustados
yhat.6= fitted(y.nnar)
yhat.6 = na.omit(yhat.6)

ypron6 = forecast(y.nnar,h=m)$mean
ypron6 = ts(ypron6,frequency=12)


(M=accuracy(ypron6,yf))


source("medidas.yest.r")

# NNAR(3,1,2)[12]: q=2 nodos en la capa, p=3 nodos de entrada 
#                  autoregresivos, 1 nodo estacional
#                  k = 1 + 2*(3+1)+ 2*1 = 11

(A=medidas.yest(yi,yhat.6,11))



mod7.m <- hw(yi, seasonal="add",damped=TRUE)
summary(mod7.m)

(par.mod7 = mod7.m$model$par[1:4])

yhat.7 = mod7.m$model$fitted
(A=rbind(A,medidas.yest(yi,yhat.7,3)))

ypron7 = forecast(mod7.m,h=m)$mean
ypron7 = ts(ypron7,frequency=12)

(M=rbind(M,accuracy(ypron7,yf)))


#------------------------
plot(fecha[1:(n-m)],yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 2,col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha[13:(n-m)],yhat.6,col='red')
lines(fecha[1:(n-m)],yhat.7,col='blue')


tf = seq((n-m+1),n)

plot(tf,yf, type = 'o')
lines(tf,ypron6, type = 'b', pch = 5,col='blue' )
lines(tf,ypron7, type = 'b', pch = 5,col='red' )
legend("topleft", 
c("Obs", "NNAR", "Max.ver-Holt-Winters"), 
pch = c(1, 3,  5),
col = c("black","blue","red"))
