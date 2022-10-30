# Development of project No. 1 Forecasting Techniques
# Authors: Camilo Cabrera - John Mario Montoya

# For run mario: "H:\\My Drive\\UN_Analytics_Specialization\\Forecasting_Techniques\\Projects\\project1\\src"

# Installing libraries
install.packages("ggplotw")
install.packages("fpp2")
install.packages("DescTools")

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

# Component using moving averages
components_yi <- decompose(yi)
plot(components_yi)

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

M.exp_lin <- medidas(mod.exp_lin, yi, 2)

(M.exp_lin)

yhat_exp_lin <- fitted(mod.exp_lin)
yhat_exp_lin <- ts(yhat_exp, frequency = 4, start = c(1956, 01))

# Visually evaluate the fitted
plot(yi,
    type = "l", col = "#000000", # Plot first time series
    ylim = c(min(yi, yhat_exp_lin), max(yi, yhat_exp_lin)),
    xlim = c(time(yi)[1], tail(time(yi), n = 1)),
    main = "Datos observados vs Valores ajustados
    Modelo Exponencial - Lineal + indicadoras",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(yhat_exp_lin, , type = "l", col = "#f30914")

# Add legend to plot
text_legend <- c("Observados", "Ajustados")
legend("bottomright",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = c("#000000", "#f30914")
)

#-----------------------------------#
#------------ Forecast -------------#
#-----------------------------------#

Itf <- seasonaldummy(yi, len_yf)
tf <- seq(T + 1, T + len_yf, 1)

tf2 <- tf * tf
Xtf <- cbind(rep(1, len_yf), tf, Itf)

pron_exp_lin <- predict(mod.exp_lin, data.frame(Xt = I(Xtf)))
y_pron_explin <- ts(pron_exp_lin, start = time(yf)[1], frequency = 4)

plot(yf,
    type = "o", col = "black", # Plot first time series
    ylim = c(min(y_pron_explin, yf), max(y_pron_explin, yf)),
    xlim = c(time(yf)[1], tail(time(yf), n = 1)),
    main = "Pronósticos Modelo Exponencial Lineal:
    Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)

lines(y_pron_explin, type = "o", col = "#f30914")

# Add legend to plot
text_legend <- c("Observados", "Ajustados")
legend("bottom",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = c("#000000", "#f30914")
)

# Metric for quality forecast MAPE, RMSE, UTHEIL
R <- rbind(accuracy(yf, y_pron_explin))[, c(2, 5)]
Utheil <- c(TheilU(yf, y_pron_explin))
R <- c(R, setNames(Utheil, "Utheil"))


#-----------------------------------------------------------------------------#
#-------------Test Model No1 Variation: Quadratic - Exponential --------------#
#-----------------------------------------------------------------------------#

mod.llin2 <- lm(lyi ~ ti + ti2 + It)

# Seasonal linear exponential model
T <- length(yi)
Xt <- cbind(rep(1, T), ti, ti2, It)
Ds <- data.frame(yi, Xt)
theta.0 <- mod.llin2$coefficient

# Use nls function for adjust
mod.exp_q <- nls(yi ~ exp(Xt %*% theta),
    data = Ds, start = list(theta = theta.0)
)

M.exp_q <- medidas(mod.exp_q, yi, 2)

(M.exp_q)

yhat_exp_q <- fitted(mod.exp_q)
yhat_exp_q <- ts(yhat_exp_q, frequency = 4, start = c(1956, 01))

# Visually evaluate the fitted
plot(yi,
    type = "l", col = "#000000", # Plot first time series
    main = "Datos observados vs Valores ajustados
    Modelo Exponencial - Cuadratico + indicadoras",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(yhat_exp_q, , type = "l", col = "#790a0a")

# Add legend to plot
text_legend <- c("Observados", "Ajustados")
legend("bottomright",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = c("#000000", "#790a0a")
)

#-----------------------------------#
#------------ Forecast -------------#
#-----------------------------------#

Itf <- seasonaldummy(yi, len_yf)
tf <- seq(T + 1, T + len_yf, 1)

tf2 <- tf * tf
Xtf <- cbind(rep(1, len_yf), tf, tf2, Itf)

pron_exp_q <- predict(mod.exp_q, data.frame(Xt = I(Xtf)))
y_pron_exp_q <- ts(pron_exp_q, start = time(yf)[1], frequency = 4)

plot(yf,
    type = "o", col = "black", # Plot first time series
    ylim = c(min(y_pron_exp_q, yf), max(y_pron_exp_q, yf)),
    xlim = c(time(yf)[1], tail(time(yf), n = 1)),
    main = "Pronósticos Modelo Exponencial Cuadrático:
    Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)

lines(y_pron_exp_q, type = "o", col = "#790a0a")

# Add legend to plot
text_legend <- c("Observados", "Ajustados")
legend("bottom",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = c("#000000", "#790a0a")
)


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

# Visually evaluate the fitted
plot(yi,
    type = "l", col = "#000000", # Plot first time series
    main = "Datos observados vs Valores ajustados
     Modelo Holt-Winters componentes",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(Yt_hat, , type = "l", col = "#0d24f3")

# Add legend to plot
text_legend <- c("Observados", "Ajustados")
legend("bottomright",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = c("#000000", "#0d24f3")
)

#-----------------------------------#
#------------ Forecast -------------#
#-----------------------------------#


y_pron <- predict(model, len_yf, prediction.interval = TRUE)

plot(yf,
    type = "o", col = "black", # Plot first time series
    ylim = c(min(y_pron[, 1], yf), max(y_pron[, 1], yf)),
    xlim = c(time(yf)[1], tail(time(yf), n = 1)),
    main = "Pronósticos Modelo Holt-Winters:
    Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(y_pron[, 1], , type = "o", col = "#0d24f3")

text_legend <- c("Observados", "Ajustados")
legend("bottomright",
    legend = text_legend, ,
    text.width = strwidth(text_legend)[1] * 2,
    lty = 1, ,
    col = c("#000000", "#0d24f3")
)

# Metric for quality forecast MAPE, RMSE, UTHEIL
R <- rbind(accuracy(yf, y_pron[, 1]))[, c(2, 5)]
Utheil <- c(TheilU(yf, y_pron[, 1]))
R <- c(R, setNames(Utheil, "Utheil"))