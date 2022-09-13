install.packages("pastecs")
library(pastecs)
library(help="datasets")
attach(airquality)
help(airquality)


# Daily readings of the following air quality values for May 1, 
# 1973 (a Tuesday) to September 30, 1973. 
# Ozone: Mean ozone in parts per billion from 1300 to 1500 hours at 
#  Roosevelt Island

str(airquality)
# 'data.frame':   153 obs. of  6 variables:
#  $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
#  $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
#  $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
#  $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
#  $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
#  $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...

# Nótese que la serie Ozone tiene datos faltantes, NA.
# El procedimiento de imputación es con base en 
# interpolación mediante splines.

n = length(Ozone)
x = seq(1,n)

ozone.lleno = regul(x, y=Ozone, 
xmin=min(x), n=length(x), 
units="days", deltat=1,
methods="spline", 
rule=2, f=0.5, periodic=FALSE,
window=(max(x) - min(x))/(n - 1), 
split=100, specs=NULL)

str(ozone.lleno)
 layout(1:2)
 ts.plot(Ozone)
 ts.plot(ozone.lleno$y$Series,ylab="imputada")