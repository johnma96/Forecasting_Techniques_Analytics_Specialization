#----------Ejemplo A.6.5 lectura archivo excel
#----------------------------------------
install.packages("readxl")
library(readxl)
res <- read_excel("international-petroleum-world-cr.xlsx")
attach(res)

# El comando plot.ts(D) grafica varias series 
#(hasta 10 máximo), que deben estar en una matriz ó 
# data.frame, por ejemplo, D.

str(res)
Classes 'tbl_df', 'tbl' and 'data.frame':       486 obs. of  5 variables:
 $ fecha : POSIXct, format: "1973-01-01" ...
 $ mexico: num  453 458 458 463 468 468 463 463 463 473 ...
 $ golfo : num  20051 20335 20332 20241 21369 ...
 $ noopec: num  25378 25627 25654 25812 25913 ...
 $ total : num  54389 54930 54995 55049 56323 ...
plot.ts(res[,c(2,3,4,5)])

# Pero se pueden comparar las series utilizando el 
# comando lines.
#-----------------------grafica
np = nrow(res)
#-----------------------convierte fecha a formato de R
fechas = as.Date(fecha,format="%Y/%m/%d")
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,res$golfo, xaxt="n", panel.first = grid(),
type='l',lwd=2,ylab='produccion.mes.',ylim=c(9000,45000))
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fechas,res$noopec,col='red',lwd=2)