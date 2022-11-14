# Development of project No. 3 Forecasting Techniques

#-----------------------------------------------------------------------------#
#------------------------- Code to get initial data -------------------------#
#-----------------------------------------------------------------------------#

# For mario enviroment: "H:\\My Drive\\UN_Analytics_Specialization\\Forecasting_Techniques\\Projects\\project3\\src"

# Libraries
library(ggplot2)
require(fpp2)
require(DescTools)

# Remove variables on memory
rm(list = ls())

# Functions for evaluate statistics of model selection
medidas.yest <- function(y, yest, k) {
    # y = serie, m = modelo, k = numero parametros
    T <- length(y)
    sse <- sum((yest - y)^2)
    ssr <- sum((y - mean(y))^2)
    mse <- sse / (T - k)
    R2 <- 1 - sse / ssr
    Ra2 <- 1 - (T - 1) * (1 - R2) / (T - k)
    aic <- log((T - k) * exp(2 * k / T) * mse / T)
    bic <- log(T^(k / T) * (T - k) * mse / T)

    M <- c(mse, sqrt(mse), Ra2, aic, bic)
    names(M) <- c("MSE", "RMSE", "R2-ajus", "logAIC", "logBIC")
    return(M)
}

medidas <- function(m, y, k) {
    # m = objeto producido con lm()
    # y = variable dependiente
    # k = numero de coeficientes beta
    T <- length(y)
    yest <- fitted(m)
    sse <- sum((yest - y)^2)
    ssr <- sum((y - mean(y))^2)
    mse <- sse / (T - k)
    R2 <- 1 - sse / ssr
    Ra2 <- 1 - (T - 1) * (1 - R2) / (T - k)
    aic <- log((T - k) * exp(2 * k / T) * mse / T)
    bic <- log(T^(k / T) * (T - k) * mse / T)
    M <- c(mse, sqrt(mse), Ra2, aic, bic)
    names(M) <- c("MSE", "RMSE", "R2-ajus", "logAIC", "logBIC")
    return(M)
}

# Load data
# Total quarterly beer production in Australia (in megalitres)
# From 1956:Q1 to 2008:Q3
y <- ts(ausbeer, frequency = 4, start = c(1956, 01)) # frecuency = 4 --> Quarters
ts.plot(y)

#----------------------------#
#----- cross-validation -----#
#----------------------------#


n <- length(y)
len_yf <- 4 * 3 # 4 * num years for test data (10 in this case) aprox 80-20
yi <- ts(y[1:(n - len_yf)], start = c(time(y)[1]), frequency = 4)
yf <- ts(y[(n - len_yf + 1):n],
    start = c(tail(time(yi), n = 1) + 0.25),
    frequency = 4
)

str(n)
str(length(yi))
str(length(yf))

# Plot data in train and test
plot(yi,
    type = "l", col = "black", # Plot first time series
    ylim = c(min(yi, yf), max(yi, yf)),
    xlim = c(time(y)[1], tail(time(y), n = 1)),
    main = "Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(yf, , type = "l", col = "#ff7300") # Plot second time series


# Add legend to plot
text_legend <- c("Train", "Test")
legend("bottomright",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = 1:2
)


#-----------------------------------------------------------------------------#
#-------------------------------- Project 3 ----------------------------------#
#-----------------------------------------------------------------------------#


library(forecast)
# install.packages("CombMSC_1.4.2.1.tar.gz", repos = NULL, type ="source")
library(CombMSC)

# ----------------------------------------------------------------------------#
# --------------------- Hyphotesis test: Canova-Hansen --------------------- #
# ----------------------------------------------------------------------------#


#-----eliminar la tendencia con stl()
m1 = stl(yi, s.window = 'per')
s1 = m1$time.series[,1]; e1 = m1$time.series[,3];

# --- Primer modelo sumando estacionalidad y error de stl
y1 = s1+e1
# --- Segundo modelo restando a la serie original el valor de tendencia de stl
y12 = yi-m1$time.series[,2]


#-----eliminar la tendencia con Loess
t = seq(1,length(yi))
mod1 = loess(yi ~ t, span=0.10,degree = 1,
control = loess.control(surface = "direct"))

yhat1 = mod1$fitted

y13 = yi-yhat1

par(mfrow=c(2,2))
plot(t,yi,type='l')
lines(t,yhat1,col='red')
lines(t,m1$time.series[,2],col='blue')
ts.plot(y1)
ts.plot(y12)
ts.plot(y13)

#-----implementacion de la prueba Canova-Hansen 
# Para encontrar necesitamos rechazar H0
require(uroot) 
res = ch.test(y1, 
type = "trigonometric",lag1 = TRUE, pvalue='raw') # lag1 = TRUE,
res

#  Canova and Hansen test for seasonal stability

# data:  y1

#       statistic pvalue
# pi/2     1.5702   0.01 **
# pi       0.4508 0.0567 .
# joint    1.6062   0.01 **
# ---
# Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Como podemos ver, se detecta raíz unitaria en pi/2 y -pi/2, aunque para pi y
# -pi muestra que el valor significancia se encuentra por encima. A pesar de 
# ello se considera que la prueba detecta raices unitarias en la serie.

# ----------------------------------------------------------------------------#
# --------------------- Hyphotesis test: Dickie-Fuller --------------------- #
# ----------------------------------------------------------------------------#


# Dickie Fuller necesita correrse con la serie normal(sin remover tendendia) ya
# que en esa tendencia está la posible raíz unitaria
# install.packages(("aTSA"))
require(aTSA)
aTSA::adf.test(yi,nlag=8)

# Type 1: no drift no trend 
#      lag     ADF p.value
# [1,]   0 -1.0753   0.294
# [2,]   1 -0.7631   0.406
# [3,]   2 -0.0266   0.636
# [4,]   3  1.8447   0.983
# [5,]   4  1.8702   0.984
# [6,]   5  1.3279   0.953
# [7,]   6  0.7577   0.861
# [8,]   7  0.9051   0.902
# Type 2: with drift no trend 
#      lag   ADF p.value
# [1,]   0 -6.14  0.0100
# [2,]   1 -5.41  0.0100
# [3,]   2 -2.97  0.0424
# [4,]   3 -2.91  0.0475
# [5,]   4 -3.20  0.0227
# [6,]   5 -2.70  0.0799
# [7,]   6 -2.33  0.2009
# [8,]   7 -2.27  0.2225
# Type 3: with drift and trend 
#      lag    ADF p.value
# [1,]   0 -7.523  0.0100
# [2,]   1 -6.689  0.0100
# [3,]   2 -3.235  0.0831
# [4,]   3 -0.614  0.9759
# [5,]   4 -0.732  0.9662
# [6,]   5 -0.847  0.9560
# [7,]   6 -1.158  0.9110
# [8,]   7 -0.896  0.9516
# ----
# Note: in fact, p.value = 0.01 means p.value <= 0.01

# Con base en la gráfica de la serie, donde se aprecia una fuerte tendencia 
# creciente y al final un decaimiento, consideramos el caso 3 con tendencia y 
# una media  diferente de cero. Aquí es apreciable que 6 de los 8 rezagos no 
# rechazan la nula de raíz unitaria (p > 0.05) y solo 2 de ellos sí lo hacen 
# los cuales además son los rezagos iniciales que no necesariamente aproximan 
# mejor la serie, por lo cual consideramos que Dickie fuller también es capaz
# de detectar la raíz unitaria.

# ----------------------------------------------------------------------------#
# -------------------------- Hyphotesis test: OCSB -------------------------- #
# ----------------------------------------------------------------------------#

#-----ocsb rechaza nula I(1,1) si tobs < tcrit
O = ocsb.test(yi)
#------rechaza Ho siendo cierta P(rechaza H0 | H0 cierta)

# OCSB test

# data:  yi

# Test statistic: -1.62, 5% critical value: -1.8927
# alternative hypothesis: stationary

# La prueba OCSB presenta un estadísico observado mayor que el valor crítico 
# (tobs > tcrit) por lo cual no rechaza la nula de raíces unitarias, por lo 
# cual al igual que las 2 pruebas anteriores, también sugiere un modelo S-ARIMA

# ----------------------------------------------------------------------------#
# -------------------------- Hyphotesis test: HEGY -------------------------- #
# ----------------------------------------------------------------------------#

## HEGY test with constant, 
# trend and seasonal dummies.

hegy.out2 = hegy.test(x=yi,
deterministic = c(1,0,0), 
lag.method = "fixed", 
maxlag = 1)
hegy.out2

#         HEGY test for unit roots

# data:  yi

#       statistic p-value
# t_1     -3.1976  0.0189 *
# t_2     -2.1796  0.0262 *
# F_3:4    1.2793  0.2936
# F_2:4    2.4525  0.0389 *
# F_1:4    4.5049   9e-04 ***
# ---
# Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Deterministic terms: constant
# Lag selection criterion and order: fixed, 1
# P-values: based on response surface regressions

# Asumiendo un modelo únicamente con media diferente de cero permitimos que la 
# prueba determine si las componentes de tendencia y la estacional son o no 
# determinísticas. Con base en el resultado vemos que el estadístico F_3:4 (+-i) 
# no resulta significativo por lo cual no rechazamos al menos una de la hipótesis 
# nulas, lo cual nos indica que sí tenemos raíz unitaria estacional.

# Concluimos a partir de las 4 prebuas de hipótesis que el modelo sí posee 
# raíces unitarias ordinarias y estacionales, y por tanto un modelo SARIMA es 
# apropiado para ajustarse.

# --------------------------------------------------------------------------- #
# --------------------------- Adjust SARIMA model --------------------------- #
# --------------------------------------------------------------------------- #

#------------------------------
dyos = diff(diff(yi,4,1),1,1)
dys = diff(yi,4,1)
dyo = diff(yi,1,1)

library(TSA)
res = armasubsets(dyos,nar=16, nma=16, y.name = "Y", ar.method = "ols")
plot(res)

# ARMA(p,d,q)(ps,D,qs)
# ARMA(4,1,1)(4,1,4)[4]
# ARMA(4,1,1)(3,1,4)[4]

# (2,1,1)(2,1,2) no nos da signicativo 1 parámetro
# (2,1,1)(0,1,1)

# estimar
m1 = arima(yi,order=c(2,1,1),
seasonal=list(order=c(0,1,1),period=4))
library(lmtest)
coeftest(m1)


yr = resid(m1)
# yr = ts(yr, frequency = 4)


plot(yr,type='o',ylab='residuo')

par(mfrow=c(3,2))
require(TSA)
plot(yr,type='o',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(yr,60,ci.type="ma",drop.lag.0 = TRUE,main="")
pacf(yr,60,main="")
qqnorm(yr)
qqline(yr,col=2)
plot(density(yr),xlab='x',main= '')
cpgram(yr) # periodograma acumulado

# La FAC permanece dentro de las bandas de barlet, y además el periodográma se
# se mantiene siempre dentro de las bandas, lo que sugiere que es RB

Box.test(x = yr, lag = 4, type="Ljung-Box")
# p-value =   0.9795, no se rechaza la nula luego hay evidencia de que es RB

Box.test(x = yr, lag = 8, type="Ljung-Box")
# p-value =  0.8767, no se rechaza la nula luego hay evidencia de que es RB

Box.test(x = yr, lag = 12, type="Ljung-Box")
# p-value =  0.9305, no se rechaza la nula luego hay evidencia de que es RB



# --------------------------------------------------------------------------- #
# --------------------------------- Forecast --------------------------------- #
# --------------------------------------------------------------------------- #

#-----------------------------------------------------------------------------#
#---------------- Model estimate project 1: Components Holt-Winters ----------------#
#-----------------------------------------------------------------------------#

#-------------------- Model with auto-tunning of parameters-------------------#

model.hw <- HoltWinters(yi)
# summary(model)
params <- (c(model.hw$alpha, model.hw$beta, model.hw$gamma))

# Trend, seasonal component and estimated Y
Tt <- model.hw$fitted[, 2] + model.hw$fitted[, 3]
St <- model.hw$fitted[, 4]
Yt_hat <- model.hw$fitted[, 1]

py.hw <- predict(model.hw, len_yf, prediction.interval = TRUE)

#-----------------------------------------------------------------------------#
#---------------- Model estimate project 3: SARIMA(2,1,1)(0,1,1)[4] ----------------#
#-----------------------------------------------------------------------------#


# pronosticar
pr = predict(m1,n.ahead=12)$pred

# Visually evaluate the prediction
par(mfrow=c(1,1))

plot(yf,
    type = "o", col = "black", # Plot first time series
    ylim = c(min(pr, yf), max(pr, yf)),
    xlim = c(time(yf)[1], tail(time(yf), n = 1)),
    main = "Pronósticos: Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)

lines(py.hw[, 1], type = "o", col = "#0d24f3")
lines(pr, type = "o", col = "#D82855")


text_legend <- c("Observados", "Ajustados HW", "Ajustados SARIMA")

legend("bottom",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 3,
    lty = 1, 
    col = c("#000000", "#0d24f3", "#D82855"),
    text.font = 1,
    pt.cex = 2,
    cex=1
)

# Metric for quality forecast MAPE, RMSE, UTHEIL
R <- rbind(accuracy(yf, pr))[, c(2, 5)]
Utheil <- c(TheilU(yf, pr))
R <- c(R, setNames(Utheil, "Utheil"))
R

#       RMSE       MAPE     Utheil 
# 9.77958646 2.17777782 0.02300116

# Aunque cuantitavimente el modelo SARIMA obtiene mejores métricas, dicha diferencia
# es muy pequeña, por lo cual consideramos que ambos modelos realizan un buen 
# trabajo en el pronóstico.