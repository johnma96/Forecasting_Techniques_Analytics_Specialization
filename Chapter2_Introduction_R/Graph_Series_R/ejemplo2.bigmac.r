#-------Ejemplo  base de datos Quandl

install.packages("devtools")
library(devtools)
install_github("quandl/quandl-r")
library(Quandl)



The Big Mac Index is an informal measure of currency exchange rates 
at ppp. It measures their value against a similar basket of goods 
and services, in this case a Big Mac. 
Differing prices at market exchange rates would imply that 
one currency is under or overvalued.

Freq: biyear

Quandl.search("bigmac")

bigmac.col = Quandl.dataset.get("ECONOMIST/BIGMAC_COL", list(rows=37))
bigmac.bra = Quandl.dataset.get("ECONOMIST/BIGMAC_BRA", list(rows=37))


Date = rev(bigmac.col$Date)

local_price.col = rev(bigmac.col$local_price)

dollar_ex.col  = rev(bigmac.col$dollar_ex )

dollar_price.col = rev(bigmac.col$dollar_price)

dollar_ppp.col  = rev(bigmac.col$dollar_ppp )


local_price.bra = rev(bigmac.bra$local_price)

dollar_ex.bra  = rev(bigmac.bra$dollar_ex )

dollar_price.bra = rev(bigmac.bra$dollar_price)

dollar_ppp.bra  = rev(bigmac.bra$dollar_ppp )


#-----------------------grafica
np = length(local_price.col)
#-----------------------convierte fecha a formato de R
fechas = as.Date(Date,format="%Y-%m-%d")

ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

par(mfrow=c(1,1))
plot(fechas,dollar_price.col, xaxt="n", panel.first = grid(),
type='l',ylab='precio big mac en USD tasa pais',
ylim = c(0,10))
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fechas,dollar_price.bra, col = 'red')




