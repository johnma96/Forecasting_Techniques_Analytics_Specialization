#https://fred.stlouisfed.org/series/ODCNPI03DEQ156N?utm_source=series_page&utm_medium=related_content&utm_term=other_formats&utm_campaign=other_format
#Categories > International Data > Countries > Germany

D = read.csv("residentialbuildingpermitsgermany.fred.csv", header=TRUE)
y = ts(D$ODCNPI03DEQ156N,frequency=4,start=c(1979,01))
ts.plot(y)


require(trend)

res <- smk.test(y)
summary(res)

#-----implementacion de la prueba ADF
require(aTSA)
adf.test(y)

#-----implementacion de la prueba Canova-Hansen

#-----eliminar la tendencia con stl()
m1 = stl(y, s.window = 'per')
T = m1$time.series[,2] 
y1 = y-T
ts.plot(y1)

require(uroot) 
ch.out1 = ch.test(y1, 
type = "trigonometric", pvalue='raw')
ch.out1


#-------implementacion de la prueba hegy

hegy.out2 = hegy.test(x=y1,
deterministic = c(0,0,1), 
lag.method = "fixed", 
maxlag = 1)

hegy.out2

#--------implementacion de la prueba ocsb
require(forecast)

# rechaza la nula de raiz unit est y ord si
# estadistico menor que valor critico

ocsb.test(y)
ocsb.test(y1)

B = mat.or.vec(4,4)
B = matrix(rep(999,16),4,4)
colnames(B) = c("I(0,0)","I(1,0)","I(0,1)","I(1,1)")
rownames(B)= c("ADF","CH","HEGY","OCSB")
B[1,2] = 1
B[2,3] =1
B[3,2] = 1
B[3,3] = 1
B[4,4] = 0

(B)




