
# Análisis de la Serie Ventas al Menudeo (Diebold, sección 4.5)
# Objetivos
# Estimar 4 modelos: lineal, cuadratico, cubico, exponencial
# Calcular pronosticos
# Decidir cual modelo ajusta mejor y cual tiene mejores pronosticos

# cargar librerias y funciones
library(forecast)


# leer los datos

D = read.table("ventas_al_menudeo.dat",header=T)
attach(D) # utiliza el nombre de las columnas como variables

# no hay variable con fechas : mensual 01/1955 hasta 12/1994
# para 469 obs, des-estacionalizadas
# RTTR es el volumen de ventas en grandes almacenes en usd de 1999
# La variable RTRR del archivo tiene datos faltantes NA al inicio 
# y al final

y = na.omit(RTRR)/10000

# Convertir los datos en un objeto tipo ts
y = ts(y,frequency=12,start=c(1955,01))

# generar un vector de fechas, clase 'Date' 
fechas = seq(as.Date("1955/1/1"), length.out = length(y), by =  "months")

# grafica con fechas
ts.plot(y,main="Ventas al menudeo en UDS de 1999")

# otros comandos para graficar con fechas con mas detalle: mes-año

np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,y, xaxt="n", panel.first = grid(),type='l',
ylab='ventas.mes', lwd = 2)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)


# Generar datos para validacion cruzada: dejar el ultimo año

T = length(y)
yi = y[1:(T-12)]
yf = y[(T-12+1):T]

# Ajustar 4 modelos: lineal, cuadrático, cúbico, log-lin

t = seq(1:(T-12))
t2 = t^2
t3 = t^3
lyi = log(yi)

# estimacion por minimos cuadrados ordinarios
mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)

summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)


# El modelo exponencial es no lineal

# paso 1) estimar el modelo log-lineal auxiliar
mod.llin = lm(lyi~t) 

# paso 2) guardar los datos en un data.frame
Ds = data.frame(yi,t)

# paso 3) guardar los parametros del log-lineal
b0.est = mod.llin$coefficient[1]
b1.est = mod.llin$coefficient[2]

# paso 4) usar la funcion nls

mod.exp = nls(yi~exp(beta0+beta1*t),
data=Ds,
start=list(beta0=b0.est, beta1=b1.est))

summary(mod.exp)

# Comparar

source("medidas.r")

M.lin = medidas(mod.lin,yi,2)
M.cuad = medidas(mod.cuad,yi,3)
M.cub = medidas(mod.cub,yi,4)
M.exp =  medidas(mod.exp,yi,2)

M = cbind(M.lin,M.cuad,M.cub,M.exp)
rownames(M) = c("mse","R2-aj","AIC","BIC")
colnames(M) = c("lin","cuad","cub","exp-lin")

require(xtable)
print(xtable(M,digits=3))

(M)


r = mod.cuad$residuals

par(mfrow=c(2,2))

plot(t,r,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r),xlab='x',main= '')
qqnorm(r)
qqline(r,col=2)
acf(r,ci.type="ma",60)


# calcular los valores ajustados con los cuatro modelos

yest = mat.or.vec(length(yi),4)
yest[,1] = fitted(mod.lin)
yest[,2] = fitted(mod.cuad)
yest[,3] = fitted(mod.cub)
yest[,4] = fitted(mod.exp)

colnames(yest) = c("lin","cuad", "cub", "exp")

# graficar las series ajustadas versus observadas
par(mfrow=c(1,1))

plot(t,yi,type = 'l',col='darkgray',lwd=2)
lines(t,yest[,1],col='red')
lines(t,yest[,2],col='blue')
# lines(t,yest[,3],col='green')
lines(t,yest[,4],col='magenta')


# Pronósticos sin incluir el cubico


tt=seq((T-12+1),T,1)
tt2 = tt*tt
pr = mat.or.vec(length(tt),3)

pr[,1] = predict(mod.lin,data.frame(t=tt))

pr[,2] = predict(mod.cuad,data.frame(t=tt,t2=tt2))

pr[,3] = predict(mod.exp,data.frame(t=tt))

# graficas de los pronosticos

par(mfrow=c(1,1))
plot(tt,yf,type='b',lwd = 2,
ylim = c(10,22))
lines(tt,pr[,2],col='blue',lwd=2)
lines(tt,pr[,1],col='red')
lines(tt,pr[,3],col='magenta')


# compara pronosticos

# calcula el mape con la funcion accuracy() de la libreria forecast

R = rbind(accuracy(yf,pr[,1]),
accuracy(yf,pr[,2]),
accuracy(yf,pr[,3]))

rownames(R) = c("M.lin","M.cuad","M.exp")

# compara calidad de pronosticos con calidad de ajuste
(R)


require(DescTools)

Utheil=c(TheilU(yf,pr[,1], type=2),
TheilU(yf,pr[,2], type=2),
TheilU(yf,pr[,3], type=2))

R = cbind(R,Utheil)

R = R[,-c(1,4)]

(R)

print(xtable(R,digits=3))

# grafica ajuste dentro de la muestra y pronostico

tc = seq((T-48),T)
yc = y[(T-48):T]


plot(tc,yc,type = 'b',ylim=c(14,22), 
col='darkgray',lwd=2, xlab='mes',ylab='ventas')
lines(tt,pr[,2],col='blue',lwd=2)
lines(tt,pr[,3],col='magenta')
abline(v=(T-11))
lines(tc,c(yest[seq((T-48),(T-12)),2],pr[,2]),col='blue',lwd=2)
lines(tc,c(yest[seq((T-48),(T-12)),4],pr[,3]),col='magenta')

DetLifeInsurance
