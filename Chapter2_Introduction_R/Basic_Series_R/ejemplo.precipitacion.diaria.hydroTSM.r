#--------Ejemplo series
library(hydroTSM)

Analysis of Daily Precipitation Data 

Loading daily precipitation data at the station 
San Martino di Castrozza, Trento Province, Italy, with
data from 01/Jan/1921 to 31/Dec/1990.

data(SanMartinoPPts)
str(SanMartinoPPts)


x <- window(SanMartinoPPts, start=as.Date("1988-01-01"))

hydroplot(x, ptype="ts", pfreq="o", var.unit="mm")

Counting and plotting the number of days in the period
 where precipitation is > 10 [mm]


( R10mm <- length( x[x>10] ) )

plot(time(x[x>10]), as.numeric(x[x>10]),type='h')
