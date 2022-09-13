
#---------Ejemplos de librarias con datos con series de tiempo

library(tswge)

data(package = 'tswge')


#-----------------------------------
library(Tcomp)
provides data from the 2010 IJF Tourism Forecasting Competition.

data(package = 'Tcomp')

data(tourism)

str(tourism)

 $ M43 :List of 9
  ..$ st         : chr "m43"
  ..$ period     : chr "MONTHLY"
  ..$ x          : Time-Series [1:306] from 1980 to 2005: 12 144 176 176 168 96 34 20 64 420 ...
  ..$ xx         : Time-Series [1:24] from 2006 to 2007: 8966 9112 6292 5372 4615 ...
  ..$ h          : num 24
  ..$ n          : num 306
  ..$ sn         : chr "M43"
  ..$ type       : chr "TOURISM"
  ..$ description: chr "No description available"
  ..- attr(*, "class")= chr "Mdata"

x = tourism$M43$x


#-----------------------grafica
np = length(x)
#-----------------------convierte fecha a formato de R
# generar un vector de fechas, clase 'Date' 
fechas = seq(as.Date("1980/1/1"), length.out = length(x), by =  "months")



ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

par(mfrow=c(1,1))
plot(fechas,x, xaxt="n", panel.first = grid(),
type='l',ylab='serie turismo')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#-----------------------------------
library(fpp3)

data(package = 'fpp3')

data(aus_arrivals)

str(aus_arrivals)

aus_arrivals = as.data.frame(aus_arrivals)
#-----------------------grafica todos
library(ggplot2)
library(zoo)

fmt <- "%Y-Q%q"

ggplot(aus_arrivals, aes(Quarter, Arrivals, col = Origin)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr(format = fmt)



#-----------------------series individuales

x.us = aus_arrivals$Arrivals[aus_arrivals$Origin=="US"]
x.uk = aus_arrivals$Arrivals[aus_arrivals$Origin=="UK"]
x.jp = aus_arrivals$Arrivals[aus_arrivals$Origin=="Japan"]
x.nz = aus_arrivals$Arrivals[aus_arrivals$Origin=="NZ"]

qtrseq<-seq(as.Date("1981-01-01"), by="quarter",
length.out = length(x.us))

fechas.q <-paste(as.POSIXlt(qtrseq)$year+1900, quarters(qtrseq))

library(zoo)

fechas.q <-as.yearqtr(fechas.q)

d.us = data.frame(fechas=fechas.q, x=x.us)

ggplot(d.us,aes(x=fechas,y=x)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearqtr(format = fmt)




