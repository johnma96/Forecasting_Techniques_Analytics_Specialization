#-----Ejemplo prueba Canova-Hansen
E = read.table("cementq.dat", header = TRUE)
attach(E)
y = ts(y,frequency=4,start=c(1956,1),end=c(1994,3))
ts.plot(y)
#-----eliminar la tendencia con stl()
m1 = stl(y, s.window = 'per')
s1 = m1$time.series[,1]; e1 = m1$time.series[,3];
y1 = s1+e1
ts.plot(y1)
#-----implementacion de la prueba Canova-Hansen
require(uroot) 
res = ch.test(y1, 
type = "trigonometric", pvalue='raw')
res
#-------resultado
data:  y1
      statistic pvalue   
pi/2     2.4867   0.01 **
pi       1.0541   0.01 **
joint     2.761   0.01 **  
Test type: seasonal cycles 
NW covariance matrix lag order: 4 
First order lag: no 
Other regressors: no  
P-values: interpolation in original tables 
