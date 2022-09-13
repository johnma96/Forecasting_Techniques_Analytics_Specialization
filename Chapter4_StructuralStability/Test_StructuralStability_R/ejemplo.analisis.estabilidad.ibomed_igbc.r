# ejemplo de analisis de estabilidad estructural
# al juntar las series de los rendimientos de ibomed e igbc
# autor : Norman Giraldo,  sep 05 de 2012

library(forecast)
library(tseries)
library(fMultivar)
library(lmtest)
library(strucchange)


uno = read.table("ibomed_igbc.dat",header=T,
stringsAsFactors=FALSE)
attach(uno)

x = ts(valor,frequency=250)

n = length(x)
r = log(x[2:n]/x[1:(n-1)])
r[848] = (r[847]+r[849])/2
r1 = (x[2:n]-x[1:(n-1)])/x[1:(n-1)]

y = c(x[1],x[1]*exp(cumsum(r)))
y1 = c(x[1],x[1]*exp(cumsum(r)))

# graficas iniciales 
# windows(1)
par(mfrow=c(2,1));
plot(y,xlab='t',type = 'l', ylab='Xt',main='Ibomed_IGBC')
plot(r,xlab='t',type = 'l', ylab='rt',main='Rendimientos')

np = length(y)
fechas = seq(as.Date(fechas[1],"%d/%m/%Y"), length.out=np, by="days")

fechas  = strptime(as.character(fechas), "%d/%m/%Y") 

ejex.mes = seq(as.Date(fechas[1],"%d/%m/%Y"), as.Date(fechas[np],"%d/%m/%Y"), "days")
ejex.año = seq(as.Date(fechas[1],"%d/%m/%Y"), as.Date(fechas[np],"%d/%m/%Y"),"years")

par(mfrow=c(1,1))

plot(fechas,y, xaxt="n", panel.first = grid(),type='l',
ylab='ventas.mes', lwd = 2)
axis.Date(1, at=ejex.mes, format="%d/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
abline(v=849)


par(mfrow=c(1,1))
np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "days")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas[-1],r,xaxt="n", panel.first = grid(),type='l',
ylab='ventas.mes', lwd = 2)
axis.POSIXct(1, at=ejex.mes, format="%d/%y")
axis.POSIXct(1, at=ejex.año, labels = FALSE, tcl = -0.2)
abline(v=849)

m = stl(ts(r,frequency=250),"per")
plot(m)

y = ts(r,frequency=250)

T = length(y)
t = seq(1,T)
It = fourier(y,2)

mod1 = lm(y ~ t + It)
summary(mod1)
dwtest(y ~ t + It)



#----estimacion recursiva de parametros
k = 6 + 250
n = T-k
parm = mat.or.vec(n, 6)
for(j in 1:n){
yj = y[1:(k+j)]
tj = t[1:(k+j)]
Itj = It[1:(k+j),]
mod.j = lm(yj ~ tj + Itj)
parm[j,] = t(mod.j$coefficient)
}
#-----grafica de las trayectorias 
colnames(parm)=c("beta.0",
"beta.1","delta.1","delta.2",
"delta.3","delta.4")
plot.ts(parm,main="")

par(mfrow=c(3,2))
tf = seq(1,n)
for(j in 1:6){
plot(tf,parm[,j],type='l')
abline(v=593)
}
#-----pruebas cusum graficas
par(mfrow=c(2,1))
prueba.cusum1 = efp(y ~ t + It, type = "Rec-CUSUM")
plot(prueba.cusum1)

prueba.cusum2 = efp(y ~ t + It, type = "OLS-CUSUM")
plot(prueba.cusum2)

#----pruebas cusum formales con valor p
sctest(prueba.cusum1)
sctest(prueba.cusum2)

 




