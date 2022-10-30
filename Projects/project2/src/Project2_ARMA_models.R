# Development of project No. 2 Forecasting Techniques
# Authors: Camilo Cabrera - John Mario Montoya

#-----------------------------------------------------------------------------#
#------------- Code to reproduce the selected model in project 1 -------------#
#-----------------------------------------------------------------------------#

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
#---------------- Model estimate No1: Linear - Exponential -------------------#
#-----------------------------------------------------------------------------#

lyi <- log(yi)

ti <- seq(1, length(yi))
ti2 <- ti * ti

It <- seasonaldummy(yi)

# Estimate auxiliar model log - linear
mod.llin <- lm(lyi ~ ti + It)
summary(mod.llin)

# Seasonal linear exponential model
T <- length(yi)
Xt <- cbind(rep(1, T), ti, It)
Ds <- data.frame(yi, Xt)
theta.0 <- mod.llin$coefficient

# Use nls function for adjust
mod.exp_lin <- nls(yi ~ exp(Xt %*% theta),
    data = Ds, start = list(theta = theta.0)
)

# Results
(summary(mod.exp_lin))

#-----------------------------------------------------------------------------#
#---------------- Model estimate No2: Components Holt-Winters ----------------#
#-----------------------------------------------------------------------------#

#-------------------- Model with auto-tunning of parameters-------------------#

model <- HoltWinters(yi)
# summary(model)
params <- (c(model$alpha, model$beta, model$gamma))

# Trend, seasonal component and estimated Y
Tt <- model$fitted[, 2] + model$fitted[, 3]
St <- model$fitted[, 4]
Yt_hat <- model$fitted[, 1]

# Statisticts model selection MSE, AIC, BIC R2-Adjust
# source('medidas.yest.r')
medidas.yest(yi, Yt_hat, 3) # OJO QUE DEVUELVE RMSE


#-----------------------------------------------------------------------------#
#-------------------------------- Project 2 ----------------------------------#
#-----------------------------------------------------------------------------#

require(lmtest)
require(TSA)

# Test Ljung-Box (Using at lest 2 lags) m = T/4, 2s, 3s; T:number of data, s:Period

#- Structural residuals for model No.2 HW (Selected in project 1 like the best model) -#

yr = resid(model)
yr = ts(yr, frequency = 4)

par(mfrow=c(3,2))
require(TSA)
plot(t,yr,type='o',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(yr,60,ci.type="ma",drop.lag.0 = TRUE,main="")
pacf(yr,60,main="")
qqnorm(yr)
qqline(yr,col=2)
plot(density(yr),xlab='x',main= '')
cpgram(yr) # periodograma acumulado

# La FAC permanece dentro de las bandas de barlet, y además el periodográma se
# se mantiene casi siempre dentro de las bandas, lo que sugiere que es RB

Box.test(x = yr, lag = 4, type="Ljung-Box")
# p-value = 0.005493, se rechaza la nula luego hay evidencia de que no es RB

Box.test(x = yr, lag = 8, type="Ljung-Box")
# p-value =  0.01618, se rechaza la nula luego hay evidencia de que no es RB

Box.test(x = yr, lag = 12, type="Ljung-Box")
# p-value =  0.041, se rechaza la nula luego hay evidencia de que no es RB


#------------------- Structural residuals for model No. 1 -------------------#
yr = resid(mod.exp_lin)
yr = ts(yr, frequency = 4)

par(mfrow=c(3,2))
require(TSA)
plot(t,yr,type='o',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(yr,60,ci.type="ma",drop.lag.0 = TRUE,main="")
pacf(yr,60,main="")
qqnorm(yr)
qqline(yr,col=2)
plot(density(yr),xlab='x',main= '')
cpgram(yr) # periodograma acumulado

# La FAC no permanece dentro de las bandas de barlett, y además el periodográma 
# se sale de las bandas, lo que sugiere que no es RB

Box.test(x = yr, lag = 4, type="Ljung-Box")
# p-value = 2.2e-16, se rechaza la nula, luego hay evidencia de que no es RB

Box.test(x = yr, lag = 8, type="Ljung-Box")
# p-value =  2.2e-16, se rechaza la nula, luego hay evidencia de que no es RB

Box.test(x = yr, lag = 12, type="Ljung-Box")
# p-value =  2.2e-16, se rechaza la nula, luego hay evidencia de que no es RB


#--------------- Modelos ARMA usando auto.arima() y armasubset ---------------#

#------------identificador armasubsets

res=armasubsets(y=yr,nar=14,nma=14,y.name='y',ar.method='ols')
par(mfrow=c(1,1))
plot(res)

# Usando el modelo HW p = 0 (1), ps = 4, q = 0 (2), qs = 0 (3)
# ARIMA(1,0,2)(3,0,0)[4] with zero mean

# Usando el modelo exponencial lineal p = 4, ps = 3, q = 0 (2), qs = 0 (3)
# ARIMA(4,0,0)(3,0,0)[4] with zero mean

#------------identificador auto.arima
auto.arima(yr)

# Usando el modelo HW
# ARIMA(1,0,2)(0,0,0)[4] with zero mean

# Usando el modelo exponencial lineal
# ARIMA(1,1,2)(1,0,1)[4] with zero mean

# ----- Tunning hyperparameters to armasubsets and comparaing both models

m.1 = arima(yr,order=c(1,0,2),seasonal=list(order=c(3,0,0),period=4))
coeftest(m.1)

m.2 = arima(yr,order=c(1,0,2),seasonal=list(order=c(0,0,0),period=4))
coeftest(m.2)

# ---- Compare AIC
(c(AIC(m.1),AIC(m.2)))

# ES NECESARIO PREGUNTAR QUÉ SUCEDE CUANDO AUTOARIMA SUGIERE UN MODELO INTEGRADO
# INICIALMENTE CAMBIÉ EL 1 A 0 EN LA PARTE NO ESTACIONAL, SIN EMBARGO ESTO 
# INCREMENTÓ EL AIC DE 1713.232 A  1724.095, SIN EMBARGO EL COEFICIENTE AR1 
# NO RESULTA SIGNIFICATIVO

at = resid(m.2)

par(mfrow=c(3,2))
plot(t,at,type='o',ylab='residuo')
abline(h=0,lty=2)
acf(at,60,drop.lag.0 = TRUE,main="")
pacf(at,60,main="")
qqnorm(at)
qqline(at,col=2)
plot(density(at),xlab='x',main= '')
cpgram(at)


Box.test(x = at, lag = 4, type="Ljung-Box")
# p-value = 0.9975, No rechaza H0 luego es RB

Box.test(x = at, lag = 8, type="Ljung-Box")
# p-value = 0.7712, No rechaza H0 luego es RB

Box.test(x = at, lag = 12, type="Ljung-Box")
# p-value = 0.8335, No rechaza H0 luego es RB

#--------------------------------- Forecast ---------------------------------#