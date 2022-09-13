
#------------ejercicios con formulas matriciales


#----------leer datos en una url
archivo = "http://www.stat.umn.edu/geyer/5102/data/ex5-3.txt"
# archivo = "ex5-3.txt"

D = read.table(archivo,header=T)
str(D)
attach(D)



#-------------estimar el modelo y=x1+x2+e, por MCO
m2 =  lm(y ~ x1 + x2)
summary(m2)

# tabla resumen
err.std = sqrt(diag(V))
cbind(beta,err.std,tStudent,valor.p)

# alternativa: summary
summary(m2)

#-----------conformar la matriz de diseño
X = model.matrix(m2)

#-----------calcular la matriz "hat"
H = X%*%solve(t(X)%*%X)%*%t(X)

#-----------valores ajustados

yhat = fitted(m2) # predicted values
yhat1 = H%*%y
head(cbind(yhat,yhat1))

par(mfrow=c(2,2))
plot(x1,y,type='p',main="A")
points(x1,yhat,col='red',pch=20)

plot(y,yhat,type='p',main="C")

#-----------calcular residuos
ehat1 = (diag(nrow(H))-H)%*%y
ehat = residuals(m2)
head(cbind(ehat,ehat1))

hist(ehat,15)

#-----------estimar mse y sigma
T = nrow(X)
(sigma.hat1 = sqrt(sum(ehat*ehat)/(T-ncol(X))))
(sigma.hat = sigma(m2))

#-----------calcular residuos estandarizados 
re1 = ehat/(sigma.hat*(rep(1,ncol(H))-diag(H)))

require(MASS)
re = stdres(m2)
head(cbind(re1,re))


plot(r,main="B")
abline(h=0)

plot(y,yhat,type='p',main="C")

plot(x2,diag(H),type='p',main="D")

# matriz de covariazas para los parametros estimados
V=vcov(m2) 

# matriz de correlaciones entre los parametros estimados
cov2cor(vcov(m2))

# estadisticos t-Student
beta = coefficients(m2)
(tStudent = beta/sqrt(diag(V)))

# valores p. Note la función pt()
nu=nrow(H)-3 # es T-k
(valor.p = pt(abs(tStudent),nu,lower.tail=FALSE)+
(pt(-abs(tStudent),nu,lower.tail=TRUE)))



