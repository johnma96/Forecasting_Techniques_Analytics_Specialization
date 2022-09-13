
# Ejemplo con modelo de componentes
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
 
library(forecast)

ti = seq(1,length(yi))

It = seasonaldummy(yi)

It.trig = fourier(yi,4)
 

#---------modelo log cuadratico con estacionalidad
ti2 = ti*ti

mod2 = lm(lyi ~ ti + ti2 + It.trig)
summary(mod2)

mod3 = lm(lyi ~ ti + ti2 + It)
summary(mod3)

(c(AIC(mod2),AIC(mod3)))



 
# El modelo exponencial-cuadratico-estacional

T = length(yi)

Xt = cbind(rep(1,T),ti,ti2,It)

Ds = data.frame(yi,Xt)

theta.0 = mod3$coefficient

mod4 = nls(yi~exp(Xt%*%theta),
data=Ds, start= list(theta=theta.0))

summary(mod4)

yhat4 = fitted(mod4)

plot(ti,yi,type='o',col='darkgray')
lines(ti,yhat4,col='red')


source("medidas.r")

(A=medidas(mod4,yi,14))

# -------pronosticos 

T = length(yi)
Itf = seasonaldummy(yi,m)
tf = seq(T+1,T+m,1)
 
# pronosticos con el exp cuad 

tf2 = tf*tf
Xtf = cbind(rep(1,m),tf,tf2,Itf)

pron4 = predict(mod4,data.frame(Xt = I(Xtf)))

par(mfrow=c(1,1))
plot(tf,yf, type = 'o')
lines(tf,pron4, type = 'b', pch = 5,col='blue' )
legend("topleft", 
c("Obs", "Exp-cuad-estac"), 
pch = c(1,  5),
col = c("black","blue"))

#-------medidas de calidad de pronosticos

pron4 = ts(pron4,frequency=12)


(M=accuracy(pron4,yf))


