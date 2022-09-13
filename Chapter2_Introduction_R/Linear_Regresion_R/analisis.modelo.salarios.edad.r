# analisis de regresión de
#  salarial anual promedio
# versus la edad del empleado público
# período 2008-2011

D = read.table("salario.mpio.med.dat",header=TRUE,stringsAsFactors=FALSE)
attach(D)

x08 = order(edad08,S08)

x = edad08[x08]
s = S08[x08]

require(ggplot2)
par(mfrow=c(1,1))
ggplot(D,aes(x = edad08, y = S08)) +  geom_point()
plot(x,s,type='p')
#------------------ variables explicativas adicionales
#------------------ es un modelo polinomico
x = as.numeric(x)
x2 = x*x
x3 = x2*x
x4 = x3*x
y = log(s) 

# transformacion logaritmica para buscar 
# que la variable y sea aproximadamente normal

#---------prueba de normalidad jarque-bera
require(tseries)
jarque.bera.test(y)
hist(y,50)


#--------------transformacion Box-Cox
#-------------- p --->  yt = (y^p - 1)/p
require(MASS)
require(car)


(p <- powerTransform(y))    # Estima p

yt <- bcPower(y,p$lambda)	# Box-Cox transfoma

par(mfrow=c(2,1))
hist(y,50)
hist(yt,50)


qqnorm(yt)

par(mfrow=c(1,1))
dx = density(yt)
plot(dx$x,dx$y,type='l')
curve(dnorm(x, mean=mean(yt), sd=sd(yt)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


#------------------------ regresion lineal (primer período)

m.08.1 = lm( y ~ x )
summary(m.08.1)

m.08.2 = lm( y ~ x + x2 )
summary(m.08.2)

m.08.3 = lm( y ~ x + x2 +x3 )
summary(m.08.3)

m.08.4 = lm( y ~ x + x2 +x3 + x4)
summary(m.08.4)

#---------------pruebas F parciales
#---------------Ho: b2 = 0, Ho: b3 = 0, Ho: b4 = 0

anova(m.08.1,m.08.2,m.08.3,m.08.4)

#-----------Ho: b2=b3=b4=0

anova(m.08.1,m.08.2,m.08.3)

#----------------resultados mejor modelo

#--------------------------
(c(AIC(m.08.1),AIC(m.08.2),AIC(m.08.3),AIC(m.08.4)))

#----------------son regresiones polinómicas
#----------------se sabe que se genera multicolinealidad
#----------------porque las potencias estan correlacionadas
#----------------alternativa: polinomios ortogonales con poly()

for(p in 1:4) {
  fm <- lm(y ~ poly(x, p))
  assign(paste("om.08", p, sep = "."), fm)
}

anova(om.08.1, om.08.2, om.08.3, om.08.4)
summary(om.08.3)
#--------------------------
(c(AIC(m.08.1),AIC(m.08.2),AIC(m.08.3),AIC(m.08.4)))

#--------------------------
(c(AIC(om.08.1),AIC(om.08.2),AIC(om.08.3),AIC(om.08.4)))


#------------------------

r08 = residuals(m.08.3)

par(mfrow=c(2,2))
ts.plot(r08,ylab='residuo',col='gray')
abline(h=0,lty=2)
plot(density(r08),xlab='x',main= '')
hist(r08,50)
qqnorm(r08)
qqline(r08,col=2)

#---------prueba de normalidad jarque-bera
require(tseries)
jarque.bera.test(r08)

#-----------------utilizar AIC, BIC


y.1 = om.08.1$fitted
y.2 = om.08.2$fitted
y.3 = om.08.3$fitted
y.4 = om.08.4$fitted

source("medidas.r")
M=cbind(
medidas(om.08.1,y,2),
medidas(om.08.2,y,3),
medidas(om.08.3,y,4),
medidas(om.08.4,y,5))


rownames(M) = c("MSE","R2-ajus","AIC","BIC")
colnames(M) = c("m1","m2","m3","m4")

(M)


#-----------------grafica y versus y ajustado(fitted)

par(mfrow=c(2,2))
plot(x,y,type='p')
lines(x,y.1,col='red',lwd=2)

plot(x,y,type='p')
lines(x,y.2,col='red',lwd=2)

plot(x,y,type='p')
lines(x,y.3,col='red',lwd=2)

plot(x,y,type='p')
lines(x,y.4,col='red',lwd=2)



#-----------------regresion robusta

m.regr1 = rlm( y ~ x + x2 + x3 + x4)
summary(m.regr1)
fregr1 = exp(m.regr1$fitted)

par(mfrow=c(1,1))
plot(x,s,type='p')
lines(x,freg08,col='red',lwd=2)
lines(x,fregr1,col='blue',lwd=2)

