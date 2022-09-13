#  manejo de datos de series de tiempo
# cargar librerias
# leer fechas y datos numericos
# asignar frecuencias
# graficar
# subconjuntos
# modelos de regresion para la tendencia
# manejo de datos faltantes

#################
# Ejemplo 1
#################

# leer la serie de tiempo como un vector numerico
y=structure(c(1574,1368,1387,1109,1257,1376,2143,1208,
2007,1876,1702,1819,1802,1205,1684,1682,1991,2394,1914,
2499,2130,2529,2328,2076,2496,1647,2518,2205,2395,2891,
2712,2427,2477,2860,2505,3355,1760,2318,3111,2570,2868,
3042,2749,2839,3140,2909,2982,3667,2814,2732,3265,3166,
2792,3742,3099,3278,4120,3553,3675,3799,3427,3234,3733,
3642,3553,3647,3624,2973,3597,3731,4092,4100,2762,3953,
4152,4229,4419,4774,4313,4060,4664,4374,4419,4908,4321,
4772,4361,4969,5111,5014,4858,5159,5086,5379,5605,5269))


# convertir a objeto ts
y=ts(y,frequency=12,start=c(1990,3))

# generar un vector de fechas, clase 'Date' 
fechas = seq(as.Date("1990/3/1"), length.out = length(y), by =  "months")

# grafica con fechas
ts.plot(y,main="serie F")

# otros comandos para graficar con fechas con mas detalle: mes-año

np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,y, xaxt="n", panel.first = grid(),type='b',ylab='produccion.mes.')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

# añadir una horizontal en el nivel 4000
abline(h = 4000, col='red') 

# cual es la fecha en la cual se supera el nivel
# 4000 por primera vez?
j=1
while( y[j] < 4000 ){
j = j + 1}

# respuesta
fechas[j]

# añadir una vertical en esta fecha 
points(fechas[j],y[j],type='h',col='blue')


# definir un consecutivo

t = seq(1,length(y))

# graficar sin fechas
plot(t,y,type='l')

# graficar la regresion lineal ajustada
abline(lm(y~t), col="red") # regression line (y~x)

#######################################
# Ejemplo 2. Series con datos faltantes
#######################################

rm(list = ls())

install.packages("pastecs")

library(pastecs)

library(help="datasets")

attach(airquality)
str(airquality)

# 'data.frame':   153 obs. of  6 variables:
#  $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
#  $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
#  $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
#  $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
#  $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
#  $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...


# Daily readings of the following air quality values for May 1, 
# 1973 (a Tuesday) to September 30, 1973. 
# Ozone: Mean ozone in parts per billion from 1300 to 1500 hours at 
#  Roosevelt Island

ts.plot(Ozone)

n = length(Ozone)

x = seq(1,n)

ozone.lleno = regul(x, y=Ozone, 
xmin=min(x), 
n=length(x), 
units="days", 
deltat=1,
methods="spline", 
rule=2, f=0.5, 
periodic=FALSE,
window=(max(x) - min(x))/(n - 1), 
split=100, 
specs=NULL)


str(ozone.lleno)

 layout(1:2)
 ts.plot(Ozone)
 ts.plot(ozone.lleno$y$Series)


filt = decmedian(ozone.lleno$y$Series, 
type="additive", 
order=22, 
times=1, 
ends="fill")

str(filt)

par(mfrow=c(1,1))
plot(x,ozone.lleno$y$Series,type='l')
lines(x,filt$series[,1],col='red',lwd=2)


