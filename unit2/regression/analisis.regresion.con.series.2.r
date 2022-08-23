# Ejemplos de repositorios con datos de series de tiempo en R


#------------------------------

This package contains data sets used as examples in the book 
"R in a Nutshell" from O'Reilly Media. 
For more information on this book, 
see http://shop.oreilly.com/product/0636920022008.do

Package ‘nutshell’ was removed from the CRAN repository.

#--------instrucciones para instalarlo
https://medium.com/@jmcmj/package-nutshel-in-r-3-6-3-77cc364fd66


library(nutshell)

#--------descripcion de datos en la libreria
https://rdrr.io/cran/nutshell/

#------------------------------
ham.price.ts: Ham Price Time Series

A time series objects consiting of average monthly 
retail prices per pound of ham in the United States 
between January 2001 and April 2008

data(ham.price.ts)

ts.plot(ham.price.ts)

#------------------------------

turkey.price.ts: Monthly Average Turkey Price, January 2001 to April 2008
his time series shows the average retail price of turkey 
in the United States between January 2001 and April 2008

data(turkey.price.ts)

ts.plot(turkey.price.ts)

m1 = lm(ham.price.ts~turkey.price.ts)
summary(m1)




#-------------------------------

Data from the M-competition and M3-competition are provided
in the Mcomp package. 

Tcomp provides data from the 
2010 IJF Tourism Forecasting Competition.

#-------------------------------

require(Mcomp)
data(package='Mcomp')
#-------------------------------
require(Tcomp)
data(package='Tcomp')

data(tourism )
str(tourism )

#-------------------------------
$ M97 :List of 9
  ..$ st         : chr "m97"
  ..$ period     : chr "MONTHLY"
  ..$ x          : Time-Series [1:306] from 1980 to 2005: 36 31 40 24 31 20 20 60 12 52 ...
  ..$ xx         : Time-Series [1:24] from 2006 to 2007: 2472 640 861 664 434 ...
  ..$ h          : num 24
  ..$ n          : num 306
  ..$ sn         : chr "M97"
  ..$ type       : chr "TOURISM"
  ..$ description: chr "No description available"

par(mfrow=c(2,1))
ts.plot(tourism$M97$x)

ts.plot(tourism$M98$x)

m1 = lm(tourism$M98$x~tourism$M97$x)
summary(m1)


#---------------------regresión espúrea
t = seq(0,10,0.5)
x = 2+0.3*t+arima.sim(n = length(t), 
list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
sd = sqrt(0.1796))
y = 1.2 + 2.1*t + rnorm(length(t),0,5.3)
par(mfrow=c(2,2))
plot(t,x,type='b')
plot(t,y,type='b')

m2 = lm(y ~ x)
summary(m2)