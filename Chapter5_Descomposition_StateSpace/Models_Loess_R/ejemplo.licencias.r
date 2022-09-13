# Analisis de Numero de Licencias

# leer
G = read.table("series_histo_88_mpio_jun17.prn", 
header = TRUE, stringsAsFactors = FALSE)
attach(G)

# definir como serie de tiempo
y = ts(G$Totalviv,frequency=12,start=c(2009,1))
ts.plot(y)

library(Kendall)
# detectar tendencia monotona
# Ho: no hay tendencia, Ha: tendencia monotona
MannKendall(y)

#---------------utilizar loess para estimar tendencia


m = 12
n = length(y)
yi = ts(y[1:(n-m)],frequency=12)
yf = ts(y[(n-m+1):n],frequency=12)

t = seq(1,(n-m))
yw = loess(yi ~ t, span=0.75,
control = loess.control(surface = "direct"))
yest.yw = yw$fitted


# grafica con fechas en el eje x

np = length(yi)

fecha = seq(as.Date("2009/01/01"), length.out=(n-m), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

lines(fecha,yest.yw, xaxt="n", panel.first = grid(),
type='l',col='red',lty=2,lwd=2)



