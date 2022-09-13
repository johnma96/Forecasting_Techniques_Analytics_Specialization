
# Modelo con prophet

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

fecha.i = fecha[1:(n-m)]
#--------------------descomposición con prophet
library(prophet)

history <- data.frame(ds = fecha.i,y = yi)
y.prophet <- prophet(history)

future <- make_future_dataframe(y.prophet, periods = 12,freq="month", include_history = TRUE)
y.f <- predict(y.prophet, future)

yhat.prophet = y.f$yhat[1:length(yi)]

plot(ti,yi,type='o',col='darkgray')
#lines(ti,yhat4,col='red')
lines(ti,yhat.stl, col='blue')
lines(ti,yhat.prophet, col='magenta')

(A=rbind(A,medidas.yest(yi,yhat.prophet,14)))
rownames(A) = c("exp.cuad.estacional","descomp.stl","descomp.prophet")
(A)

#--------------------pronosticos con prophet


pron.prophet = y.f$yhat[(n-m+1):n]


par(mfrow=c(1,1))
plot(tf,yf, type = 'o',lwd=2,ylim=c(9,25))
lines(tf,pron4, type = 'b', pch = 5,col='blue' )
lines(tf,pron.stl, type = 'b', pch = 3,col='red' )
lines(tf,pron.prophet, type = 'b', pch = 3,col='magenta' )


legend("topleft", 
c("Obs", "Exp-cuad-estac","stl","prophet"), 
pch = c(1,  5, 3,3),
col = c("black","blue","red","magenta"))


pron.prophet = ts(pron.prophet,frequency=12)


(M=rbind(M,accuracy(pron.prophet,yf)))
rownames(M) = c("exp.cuad.estacional","descomp.stl","descomp.prophet")
(M)




