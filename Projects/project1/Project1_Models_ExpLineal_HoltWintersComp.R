# Development of project No. 1 Forecasting Techniques 
# Authors: Camilo Cabrera - John Mario Montoya

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
    names(M) <- c("mse", "rmse", "R2-ad", "log.aic", "log.bic")
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
    M <- c(, mse, sqrt(mse), Ra2, aic, bic)
    names(M) <- c("MSE", "RMSE", "R2-ajus", "logAIC", "logBIC")
    return(M)
}

# Load data
# Total quarterly beer production in Australia (in megalitres)
# From 1956:Q1 to 2008:Q3
y <- ts(ausbeer, frequency = 4, start = c(1956, 01)) # frecuency = 4 --> Quarters
ts.plot(y)

#----- cross-validation
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
lines(yf, , type = "l", col = "red") # Plot second time series


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

lyi = log(yi)

ti = seq(1,length(yi))

# Estimate auxiliar model log - linear
mod.llin = lm(lyi~ti)

# save parameters of model log-linear
b0.est = mod.llin$coefficient[1]
b1.est = mod.llin$coefficient[2]

# Save data in dataframe
Ds = data.frame(yi,ti)

# Use nls function for adjust 
mod.exp = nls(yi~exp(beta0+beta1*ti),
    data=Ds,start=list(beta0=b0.est, beta1=b1.est))

# Results 
str(summary(mod.exp))

M.exp = medidas(mod.exp,yi,2)

str(M.exp)


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
    main = "Y real vs modelo Holt-Winters Componentes",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(Yt_hat, , type = "l", col = "#f3a60d")

#----------------#
#--- Forecast ---#
#----------------#
y_pron <- predict(model, len_yf, prediction.interval = TRUE)

plot(y,
    type = "l", col = "black", # Plot first time series
    main = "Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(Yt_hat, , type = "l", col = "#f20505")

# Visually evaluate the prediction
lines(yf, , type = "l", col = "#000000")
lines(y_pron[, 1], , type = "l", col = "orange")

# Metric for quality forecast MAPE, RMSE, UTHEIL
R <- rbind(accuracy(yf, y_pron[, 1]))[, c(2, 5)]
Utheil <- c(TheilU(yf, y_pron[, 1]))
R <- c(R, setNames(Utheil, "Utheil"))
# rownames(R) <- c("HWC")
