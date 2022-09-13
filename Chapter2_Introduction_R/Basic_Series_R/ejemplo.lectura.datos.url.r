#---Ejemplo A.6.4 lectura de una URL
# Ver https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# An example is a data set of the number of births per month in New York city, 
# from January 1946 to December 1959


archivo = "http://robjhyndman.com/tsdldata/data/nybirths.dat" 
G = read.table(archivo)
str(G)
x = ts(G,frequency=12,start=c(1946,1))

#-----------------------grafica
np = length(x)
#-----------------------convierte fecha a formato de R
fechas = seq(as.Date("1946/01/01"), as.Date("1959/12/31"), by="months")
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,x, xaxt="n", panel.first = grid(),
type='b',ylab='numero recien nacidos')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#---------lectura archivos .csv
archivo = "https://stats.idre.ucla.edu/stat/data/poisson_sim.csv"
H = read.csv(archivo)
str(H)

#----grafica trayectorias
N = H$num_awards
plot(time(N),N,type='s')

#----grafica datos discretos en tabla

I = H$math
plot(table(I))