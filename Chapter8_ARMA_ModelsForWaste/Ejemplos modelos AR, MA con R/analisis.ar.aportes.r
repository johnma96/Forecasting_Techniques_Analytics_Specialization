# Ejemplo de analisis AR(p)
# Serie de aportes hidrológicos diarios
# en Gw/h
# 

library(itsmr)
library(forecast)
library(TSA)
library(lmtest)
library(tseries)
library(FitAR)
library(fArma)
library(timsac) # autoarmafit

#-----------------datos
D = read.table("Precio.Aportes.prn", header = T, stringsAsFactors=FALSE)
attach(D)
 
#-----------------grafica
fechas = as.Date(Fecha,format="%d/%m/%Y")
np = length(Ap)

ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,Ap, xaxt="n", panel.first = grid(),type='l'
,ylab='daily water inflows', xlab='date', main="(a)")
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

# https://stats.stackexchange.com/questions/
# 1207/period-detection-of-a-generic-time-series
 
	
Rob Hydman:
"If you really have no idea what the periodicity is, 
probably the best approach is to find the frequency 
corresponding to the maximum of the spectral density. 
However, the spectrum at low frequencies will be affected by trend, 
so you need to detrend the series first. 
The following R function should do the job for most series. 
It is far from perfect, but I've tested it on a few dozen 
examples and it seems to work ok. 
It will return 1 for data that have no strong periodicity,
 and the length of period otherwise."


source("find.freq.r")
find.freq(Ap)



#-----------------modela estacionalidad con sen, cos
#---------------- serie con frecuencia diaria

require(forecast)

Ap = ts(Ap,frequency = 365)

It.trig = fourier(Ap,4)

t = seq(1,length(Ap))/100

mod2 = lm(Ap ~ t + It.trig)
summary(mod2)

#-----------------examen fac y fac parcial residuos
r = residuals(mod2)
require(TSA)
par(mfrow=c(1,2))
TSA::acf(r,60, drop.lag.0 = TRUE,ci.type="ma",main="fac")
pacf(r,60,main="fac parcial")

#-----------------identifica AR(p)
install.packages("FitAR_1.94.tar.gz", 
repos = NULL, type="source")
require(FitAR)

n = length(Ap)
pvec = SelectModel(r, ARModel="AR", 
Criterion="BIC", 
lag.max=floor(10*log10(n)), Best=1)
(p=pvec)

#-----------------estima AR(p)
ar(x, aic = TRUE, order.max = floor(10*log10(n)),
   method = c("yule-walker", "burg", "ols", "mle", "yw"),
   na.action, series, ...)

# aic Logical flag. If TRUE then the Akaike Information Criterion is used 
# to choose the order of the autoregressive model. If FALSE, 
# the model of order order.max is fitted.

mod1 = ar(r,aic = TRUE, order.max = floor(10*log10(n)), method=c("burg"))
print(mod1)


require(tseries)

mod1.1 = arma(r,order=c(30,0), include.intercept = FALSE)
summary(mod1.1)

#-----------------chequeo estacionario raices fuera circulo unidad

phi = mod1$ar #fitted parameters
phi1.1 = mod1.1$coef
cbind(phi,phi1.1)

# de FitAR
InvertibleQ(phi)

#-----------------grafica raíces autorregresivas

source("armaRoots.r")
armaRoots(phi)

    require(signal)
    zplane(filt=c(1),a=rev(c(1,-phi)))

#----------------- la fac teorica

fac.teo= ARMAacf(ar=phi,ma=numeric(0),lag.max=60)

facp.teo = ARMAacf(ar=phi,
ma=numeric(0),lag.max=60,pacf=TRUE)


par(mfrow=c(1,1))
TSA::acf(r,60, drop.lag.0 = TRUE,ci.type="ma",main="fac")

points(seq(1,60),fac.teo[-1],col='red',pch=20)


#-----------------pruebas incorrelación residuos AR(p)

et = na.omit(resid(mod1))

require(hwwntest)
hwwn.test(et[1:4096])


Box.test(et,30,type= "Ljung-Box")

Box.test(et,40,type= "Ljung-Box")

Box.test(et,50,type= "Ljung-Box")

Box.test(et,60,type= "Ljung-Box")

#-----------------gráficas residuos AR(9)

par(mfrow=c(2,2))
ts.plot(et)
TSA::acf(et,180, drop.lag.0 = TRUE,ci.type="ma")
pacf(et,180)
cpgram(et)
find.freq(et)


