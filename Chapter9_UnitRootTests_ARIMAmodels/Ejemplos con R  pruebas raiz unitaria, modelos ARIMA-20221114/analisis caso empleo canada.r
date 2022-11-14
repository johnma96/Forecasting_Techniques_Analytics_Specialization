# ejemplo de analisis mediante modelos arma
# incluyendo pronosticos
# serie de empleo canada
# ver capitulo 6, paginas 123-125


library(forecast) # para la funcion auto.arima

# Canadian employment index, seasonally adjusted, 1961:1-1994:4 (136 obs)

E = read.table("CAEMP.DAT", header = TRUE)

y = ts(E$caemp, frequency = 4, start = c(1961,01), end = c(1994,04))

Dy = diff(y,1,1)

library(TSA)
par(mfrow=c(2,2))
plot(y, main="tasa empleo Canada trimestral des-estacionalizado")
acf(y,28,ci.type="ma",drop.lag.0 = TRUE)
pacf(y,28)
plot(Dy)


library(aTSA)
aTSA::adf.test(y)

#--------------------------------ejemplo calculo directo
library(dynlm)
reg.drift = dynlm(Dy ~ 1 + L(y,1) + L(Dy, 1)+ L(Dy, 2))
summary(reg.drift)
#---------
library(urca)
df.drift = ur.df(y = y, lags = 2, type = 'drift' )
summary(df.drift)

lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)

Value of test-statistic is: -2.4959 2.6195 3.9113 
Critical values for test statistics: 
      1pct  5pct 10pct
tau3 -3.99 -3.43 -3.13
#----------aTSA::adf.test(y)
     lag   ADF p.value
[3,]   2 -2.50   0.367
#----------------------------------------------
kpss.test(y, lag.short = TRUE, output = TRUE)

#----------------------------------------------
aTSA::pp.test(y, lag.short = TRUE, output = TRUE)

#----------------------------------------------
library(forecast)

auto.arima(y)
ARIMA(1,1,0) 
auto.arima(y,stationary=TRUE)
ARIMA(2,0,0) with non-zero mean


# pruebas calidad pronostico dentro de la muestra
f2 <- arima(y,order=c(2,0,0))
f1 <- arima(y,order=c(1,1,0))
(rbind(accuracy(fitted(f1),y),
accuracy(fitted(f2),y)))

dm.test(residuals(f1),residuals(f2),h=7)

# pruebas de calidad pronosticos con base en periodo entrenamiento
f1 <- arima(y[1:100],order=c(2,0,0))
f2 <- arima(y[1:100],order=c(1,1,0))

f1.out <- Arima(y[101:136],model=f1)
f2.out <- Arima(y[101:136],model=f2)
accuracy(f1.out)
accuracy(f2.out)
dm.test(residuals(f1.out),residuals(f2.out),h=1)
