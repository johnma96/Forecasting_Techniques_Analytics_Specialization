# Development of project No. 1 Forecasting Techniques 
# Authors: Camilo Cabrera - John Mario Montoya

# Libraries
library(ggplot2)
require(fpp2)

# Remove variables on memory
rm(list=ls())

# Load data
# Total quarterly beer production in Australia (in megalitres) 
# From 1956:Q1 to 2008:Q3
y = ts(ausbeer,frequency=4,start=c(1956,01)) #frecuency = 4 --> Quarters

#----- cross-validation
n = length(y)
len_yf = 4 * 10 # 4 * num years for test data (10 in this case) aprox 80-20
yi = ts(y[1:(n-len_yf)],start=c(time(y)[1]),frequency=4)
yf = ts(y[(n-len_yf+1):n],start=c( tail(time(yi),n=1) +0.25 ),frequency=4)

# Plot data in train and test
plot( yi,type="l", col="black",                   # Plot first time series
    ylim = c(min(yi,yf), max(yi,yf)),
    xlim=c(time(y)[1],tail(time(y),n=1)),
    main="Producción trimestral de cerveza en Australia",
    xlab="Año",
    ylab="Megalitros")
lines(yf,,type="l", col="red")                  # Plot second time series
text_legend = c("Train", "Test")
legend("bottomright",                          # Add legend to plot
       ,legend=text_legend,
       ,text.width = strwidth(text_legend)[1]*2
       ,lty = 1,
       ,col = 1:2
       )

# lyi = log(yi)

