    # analisis de los datos AirPassengers


	rm(list=ls())       
	graphics.off()

    library(forecast)

    source("medidas.r")


    # AirPassengers   Monthly Airline Passenger Numbers
    #             1949-1960

    data(AirPassengers)
    y = AirPassengers

    ly = log(y)

    fechas = seq(as.Date("1949/1/1"), as.Date("1960/12/1"), by="months")

    par(mfrow=c(2,1))
    ts.plot(y)
    ts.plot(ly)

#----------------validacion cruzada: otra manera
m = 12
n = length(y)
yi = ts(y[1:(n-m)],frequency=12)
yf = ts(y[(n-m+1):n],frequency=12)

lyi = log(yi)


#----modelo con tendencia lineal y estacionalidad
#----con variables indicadoras estacionales

t = seq(1,length(yi))

It = seasonaldummy(yi)

mod1 = lm(yi ~ t + It)
summary(mod1)

#print(xtable(mod1),digits=6)

#----------------valores estimados de y
yhat1 = fitted(mod1)

 
(M.lin = medidas(mod1,yi,5))




#---------modelo log cuadratico con estacionalidad
t2 = t*t
mod2 = lm(lyi ~ t + t2 + It)
summary(mod2)
 
# El modelo exponencial-cuadratico-estacional
T = length(yi)
Xt = cbind(rep(1,T),t,t2,It)
Ds = data.frame(yi,Xt)
theta.0 = coef(mod2)

mod3 = nls(yi~exp(Xt%*%theta),
data=Ds, start= list(theta=theta.0))

summary(mod3)

yhat3 = fitted(mod3)

M.exp.cuad = medidas(mod3,yi,6)
(M = cbind(M.lin,M.exp.cuad))

# -------ajuste de ambos modelos
par(mfrow=c(1,1))

plot(t,yi,type = 'b', ylim=c(0,600))
lines(t,yhat1,col='red',lwd=2)
lines(t,yhat3,col='darkgreen', lwd=2)
 

legend("topleft", 
c("Obs","cuad-estac", "Exp-cuad-estac"), 
lty = c(1, 1, 1), lwd=2,
col=c("black","red","darkgreen"))

# -------pronosticos 

T = length(yi)
Itf = seasonaldummy(yi,m)
tf = seq(T+1,T+m,1)
 
pron1 = predict(mod1,
data.frame(t = tf, It=I(Itf)))

# pronosticos con el exp cuad 

tf2 = tf*tf
Xtf = cbind(rep(1,m),tf,tf2,Itf)
pron3 = predict(mod3,
data.frame(Xt = I(Xtf)))

par(mfrow=c(1,1))
plot(tf,yf, type = 'o',ylim=c(300,700))
lines(tf,pron1, type = 'b', pch = 3,col='red' )
lines(tf,pron3, type = 'b', pch = 5,col='blue' )


legend("topleft", 
c("Obs","cuad-estac", "Exp-cuad-estac"), 
pch = c(1, 3, 5), 
cex=1.5, col=c("black","red","blue"))


A=rbind(
accuracy(ts(pron1,frequency=12),yf), 
accuracy(ts(pron3,frequency=12),yf))

rownames(A) = c("Lin-Ind","Exp-Cuad-Ind")
(A)                   

              ME     RMSE      MAE      MPE
Lin-Ind      28.769697 49.47908 37.22197 4.974570
Exp-Cuad-Ind  9.639401 19.58614 15.98574 1.758334
                 MAPE      ACF1 Theil's U
Lin-Ind      7.073539 0.6719868 0.8718742
Exp-Cuad-Ind 3.276264 0.1397661 0.3961276


# http://docs.oracle.com/cd/E17236_01
#/epm.1112/cb_statistical
#/frameset.htm?ch07s02s03s04.html


#------- estimar el efecto de cada mes
Xt = cbind(rep(1,T),t,t2)
Ds = data.frame(yi,Xt)
theta.0 = coef(mod2)[1:3]

mod4 = nls(yi~exp(Xt%*%theta),
data=Ds, start= list(theta=theta.0))

# residuos modelo exp cuad sin tendencias
St.et = residuals(mod4)

# completa la matriz con el diciembre
Dec = rep(1,T) - apply(It,1,sum)
It12 = cbind(It,Dec)

mod5 = lm(St.et ~It12-1)
summary(mod5)

plot(coef(mod5),type='b',lwd=2)
abline(h=0)
