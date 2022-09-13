
# leer
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


lyi = log(yi)

#--------------------descomposición con stl

y.stl <- stl(yi,"per")

St.stl = y.stl$time.series[,1]
Tt.stl = y.stl$time.series[,2]

yhat.stl = Tt.stl+St.stl

plot(ti,yi,type='o',col='darkgray')
lines(ti,yhat4,col='red')

lines(ti,yhat.stl, col='blue')


source("medidas.yest.r")

(A=rbind(A,medidas.yest(yi,yhat.stl,3)))
rownames(A) = c("exp.cuad.estacional","descomp.stl")
(A)

#--------------------pronosticos con stl
library(forecast)

pron.stl = forecast(y.stl,method="ets",h=12)$mean


par(mfrow=c(1,1))
plot(tf,yf, type = 'o')
lines(tf,pron4, type = 'b', pch = 5,col='blue' )
lines(tf,pron.stl, type = 'b', pch = 3,col='red' )


legend("topleft", 
c("Obs", "Exp-cuad-estac","stl"), 
pch = c(1,  5, 3),
col = c("black","blue","red"))


#-------medidas de calidad de pronosticos

pron.stl = ts(pron.stl,frequency=12)


(M=rbind(M,accuracy(pron.stl,yf)))
rownames(M) = c("exp.cuad.estacional","descomp.stl")
(M)


