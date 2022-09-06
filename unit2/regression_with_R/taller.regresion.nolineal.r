# analisis de regresión de
#  salarial anual promedio
# versus la edad del empleado público
# período 2008-2011

D = read.table("salario.mpio.med.dat",header=TRUE,stringsAsFactors=FALSE)
attach(D)

x08 = order(edad08,S08)

x = edad08[x08]
s = S08[x08]/100000
#------------------ es un modelo polinomico
x = as.numeric(x)/10
x2 = x*x

m.08.2 = lm( s ~ x + x2 )
summary(m.08.2)
theta = coefficients(m.08.2)

#------------------ 
https://stackoverflow.com/questions/18364402/r-nls-singular-gradient

m = nls(s ~ exp(b0+b1*x+b2*x2),
start=list(b0=theta[1], b1=theta[2], b2=theta[3]))
summary(m)

#------------------ 

shat=fitted(m.08.2)
snlhat=fitted(m)

plot(x,s,type='p')
lines(x,shat,lwd=2)
lines(x,snlhat,lwd=2,col='red')


#------------------ 

library(minpack.lm)

m1 = nlsLM(s ~ a0+1/(1+exp(b0+b1*x+b2*x2)),
start=list(a0=10.0,b0=theta[1], b1=theta[2], b2=theta[3]))

m1=mean(x)
m2=mean(x2)




