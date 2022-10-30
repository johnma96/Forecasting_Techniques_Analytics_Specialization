# Ejemplo simulacion basica S-ARMA(1,2)(1,1)[12]
# con programa sarima.sim
a = c(1,-0.3)
#-------------- 
b = c(1,0.07255651,- 0.6294622)
#-------------- 
ae = c(1,-0.12)
#-------------- 
be= c(1,- 0.0209915)
#---- usar la funcion sarima_sim con los coeficientes a y b.
n = 300

y <- sarima_sim( Num=n,
startprd=4, 
var=0.3,
nonseasonal=list(order=c(1,0,2), 
AR_coeffs=-a[-1],
MA_coeffs=-b[-1]),
seasonal=list(order=c(1,0,1), 
AR_coeffs=-ae[-1],
MA_coeffs=-be[-1],
period=12), init_vals= NULL )$ts_series;

ts.plot(y,main="S-ARMA(1,2)(1,1)[12], sigma^2 = 0.3")


y = ts(y,frequency=12)

#-----------verifica periodo
par(mfrow=c(2,1))
spectrum(y, spans=c(7), log="dB", ci=0.8)
spec.pgram(y,kernel("modified.daniell", c(3,3)), log="no")

#---- usar la libreria CombMSC
library(CombMSC)

y1 = sarima.Sim(n = 25, period =12, 
model=list(order=c(1,0,2),ar = -a[-1],
ma = b[-1], sd = sqrt(0.3)), 
seasonal=list(order=c(1,0,1),ar=-ae[-1],ma = be[-1]))

ts.plot(y1)

y1 = ts(y1,frequency=12)

#-----------verifica periodo
par(mfrow=c(2,1))
spectrum(y1, spans=c(7), log="dB", ci=0.8)
spec.pgram(y1,kernel("modified.daniell", c(3,3)), log="no")

require(TSA)
par(mfrow=c(2,2))
acf(y,48,ci.type="ma",drop.lag.0=TRUE)

acf(y1,48,ci.type="ma",drop.lag.0=TRUE)
pacf(y,48)
pacf(y1,48)


#----------identifica
auto.arima(y)
auto.arima(y1)

