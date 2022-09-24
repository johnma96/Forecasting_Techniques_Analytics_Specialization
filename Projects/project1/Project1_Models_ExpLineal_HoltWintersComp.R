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

# Load data
# Total quarterly beer production in Australia (in megalitres)
# From 1956:Q1 to 2008:Q3
y <- ts(ausbeer, frequency = 4, start = c(1956, 01)) # frecuency = 4 --> Quarters
ts.plot(y)

#----- cross-validation
n <- length(y)
len_yf <- 4 * 10 # 4 * num years for test data (10 in this case) aprox 80-20
yi <- ts(y[1:(n - len_yf)], start = c(time(y)[1]), frequency = 4)
yf <- ts(y[(n - len_yf + 1):n], start = c(tail(time(yi), n = 1) + 0.25),
         frequency = 4)


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

# Visually evaluate the fitted
plot(yi,
    type = "l", col = "#000000", # Plot first time series
    main = "Y real vs modelo Holt-Winters Componentes",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(Yt_hat, , type = "l", col = "#f3a60d")

# Forecast
y_pron <- predict(model, 40, prediction.interval = TRUE)

plot(y,
    type = "l", col = "black", # Plot first time series
    main = "Producción trimestral de cerveza en Australia",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(Yt_hat, , type = "l", col = "#f20505")

#Visually evaluate the prediction
lines(yf, , type = "l", col = "#000000")
lines(y_pron[,1], , type = 'l', col = "orange")

# Metric for
R <- rbind(accuracy(yi, Yt_hat)) 
rownames(R) <- c('HWC')











#-------------------- Model 2 with auto-tunning of parameters-------------------#

model1 <- HoltWinters(yi)
param1 <- (c(model1$alpha, model1$beta, model1$gamma))

# Trend, seasonal component and estimated Y
Tt <- model1$fitted[, 2] + model1$fitted[, 3]
St <- model1$fitted[, 4]
Yt_hat <- model1$fitted[, 1]

plot(Yt_hat,
    type = "l", col = "#1164a3", # Plot first time series
    main = "Y real vs modelo Holt-Winters Componentes",
    xlab = "Año",
    ylab = "Megalitros"
)
lines(yi, , type = "l", col = "red")

# Forecast of 12
y_pron <- predict(model1, 12, prediction.interval = TRUE)


