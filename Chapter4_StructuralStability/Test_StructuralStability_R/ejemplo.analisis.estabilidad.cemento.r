# ejemplo de analisis de estabilidad estructural
# en la serie de produccion de cemento , 
# autor: Norman Giraldo,  sep 5 de 2012

library(xtable)

rm(list=ls())       
graphics.off()


archivo = "cementq.dat"


E = read.table(archivo, header = TRUE)
attach(E)
   
y = ts(y,frequency=4,start=c(1956,1),end=c(1994,3))
 

#----modelo con tendencia lineal y estacionalidad
library(forecast)

T = length(y)
t = seq(1,T)
It = seasonaldummy(y)

mod1 = lm(y ~ t + It)
summary(mod1)


library(lmtest)

dwtest(y ~ t + It)



#----estimacion recursiva de parametros
k = 2 + frequency(y) -1 + 10
n = T-k
parm = mat.or.vec(n, 5)
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
"delta.3")
plot.ts(parm,main="")

#-----pruebas cusum graficas

library(strucchange)

par(mfrow=c(2,1))
prueba.cusum1 = efp(y ~ t + It, type = "Rec-CUSUM")
plot(prueba.cusum1)

prueba.cusum2 = efp(y ~ t + It, type = "OLS-CUSUM")
plot(prueba.cusum2)

#----pruebas cusum formales con valor p
sctest(prueba.cusum1)
sctest(prueba.cusum2)


#----encontrar el punto de quiebre

bp.n = breakpoints(y ~ t + It, breaks=3)
summary(bp.n)

#--------note que bp.n$extract.breaks es una funcion

B= bp.n$extract.breaks(bp.n$RSS.table,breaks=3)
str(B)
 


#---- grafica de cusum con punto de quiebre
par(mfrow=c(2,2))

rcres = recresid(y ~ t + It)
plot(cumsum(rcres),type='l')
abline(v=B[1],lty=2,lwd=2)
abline(v=B[2],lty=2,lwd=2)
abline(v=B[3],lty=2,lwd=2)
#----grafica de residuos OLS con punto de quiebre
r1 = mod1$residuals

plot(t,r1,type='l',ylab='residuo')
abline(v=B[1],lty=2,lwd=2)
abline(v=B[2],lty=2,lwd=2)
abline(v=B[3],lty=2,lwd=2)

#----grafica de la serie con punto de quiebre
plot(t,y,type='l',ylab='serie')
abline(v=B[1],lty=2,lwd=2)
abline(v=B[2],lty=2,lwd=2)
abline(v=B[3],lty=2,lwd=2)

 
