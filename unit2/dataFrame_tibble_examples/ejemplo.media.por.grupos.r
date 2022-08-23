# https://stackoverflow.com/questions/21982987/
# mean-per-group-in-a-data-frame

# Ejemplo de media por grupos en un data.frame 
# con la funcion  aggregate

D = read.table(text=
'Nombre     Mes  Rate1     Rate2
Aira       1      12        23
Aira       2      18        73
Aira       3      19        45
Ben        1      53        19
Ben        2      22        87
Ben        3      19        45
Cat        1      22        87
Cat        2      67        43
Cat        3      45        32', header=TRUE)

# ayuda de la funcion aggregate
?aggregate

# media por Nombre
aggregate(D[, 3:4], list(D$Nombre), mean)

# desviacion estandar por Nombre
aggregate(D[, 3:4], list(D$Nombre),sd)

# coeficiente de variacion Cv = sd/mean por mes?

Mmes = aggregate(D[, 3:4], list(D$Mes), mean)
DSmes = aggregate(D[, 3:4], list(D$Mes),sd)

(Cv = DSmes/Mmes)

