
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

ti = seq(1,length(yi))

mod.loess = loess(yi ~ ti, span = 0.75,degree = 1,
control = loess.control(surface = "direct"))

yhat.loess = mod.loess$fitted

rt = yi-yhat.loess

#----------generar matriz estacional con 12 columnas
It.12 = cbind(It,rep(0,length(yi)))
It.12[,12] = rep(1,length(yi)) - apply(It,1,sum)

#----------regresion con la componente estacional 
mod.hib = lm(rt ~ It.12-1)
summary(mod.hib)

#-----------valores ajustados
yhat.est = mod.hib$fitted
yhat.hib = mod.loess$fitted+yhat.est


plot(ti,yi,type='o',col='darkgray',lwd=2)
lines(ti,yhat4,col='red')
lines(ti,yhat.stl, col='blue')
lines(ti,yhat.prophet, col='magenta')
lines(ti,yhat.hib, col='orange',lwd=2)


(A=rbind(A,medidas.yest(yi,yhat.hib,14)))
rownames(A) = c("exp.cuad.estacional",
"descomp.stl","descomp.prophet"
,"descomp.hibrida")
(A)

#--------------------pronosticos con hibrido

pron.loess = predict(mod.loess, data.frame(ti = tf))

Itf.12 = seasonaldummy(yi,m)
Itf.12 = cbind(Itf.12,rep(0,m))
Itf.12[,12] = rep(1,m) - apply(Itf.12,1,sum)


pron.est = predict(mod.hib, 
data.frame(It.12=I(Itf.12)))

pron.hib = pron.loess+pron.est


par(mfrow=c(1,1))
plot(tf,yf, type = 'o',lwd=2,ylim=c(9,25))
lines(tf,pron4, type = 'b', pch = 5,col='blue' )
lines(tf,pron.stl, type = 'b', pch = 3,col='red' )
lines(tf,pron.prophet, type = 'b', pch = 3,col='magenta' )
lines(tf,pron.hib, type = 'b', pch = 3,col='orange' )


legend("topleft", 
c("Obs", "Exp-cuad-estac","stl","prophet","hibrido"), 
pch = c(1,  5, 3,3,3),
col = c("black","blue","red","magenta","orange"))


pron.hib = ts(pron.hib,frequency=12)


(M=rbind(M,accuracy(pron.hib,yf)))
rownames(M) = c("exp.cuad.estacional",
"descomp.stl","descomp.prophet","descomp.hib")
(M)




