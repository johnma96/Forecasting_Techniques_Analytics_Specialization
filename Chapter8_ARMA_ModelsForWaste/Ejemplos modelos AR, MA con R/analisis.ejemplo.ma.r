# ejemplo modelo de descomposicion
# con errores ma

library(forecast)

D = read.table("ejemplo.ma.dat",header=TRUE,stringsAsFactors=FALSE)
attach(D)


y = ts(y,frequency=12,start=c(1987,6))

m1 = stl(y,"per")

plot(m1)


T = length(y)

t = seq(1,T)
t2 = t*t
t3 = t2*t

It = fourier(y,2)

m.cub = lm(y ~  t + t2 + t3 + It)
summary(m.cub)

yhat = m.cub$fitted.values

plot(t,y,type='l')
lines(t,yhat,col='red')


r = m.cub$residuals


# analisis de los residuos  

par(mfrow=c(3,2))
plot(t,r,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r),xlab='x',main= '')
acf(r,60,ci.type="ma",main="")
pacf(r,60,main="")
qqnorm(r)
qqline(r,col=2)
cpgram(r)

Box.test(r, lag = 24, type = "Ljung-Box")

require(lmtest)

dwtest(m.cub)

m.ma =  arma(r, order = c(0,3))

summary(m.ma)

m.ma =  arima(r, order = c(0,0,3))

r.ma = na.omit(residuals(m.ma))

# analisis de los residuos  

par(mfrow=c(3,2))
plot(t,r.ma,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r.ma),xlab='x',main= '')
acf(r.ma,60,ci.type="ma",main="")
pacf(r.ma,60,main="")
qqnorm(r.ma)
qqline(r.ma,col=2)
cpgram(r.ma)

Box.test(r.ma, lag = 24, type = "Ljung-Box")


# pronosticos 12 meses 


Itp = fourierf(y,2,12)
tp = seq(T+1,T+12,1)
tp2 = tp*tp
tp3 = tp2*tp
 
pro.estruc = predict(m.cub,
data.frame(t = tp,t2 = tp2, t3 = tp3, It=I(Itp)))

pro.ma = predict(m.ma,n.ahead=12)$pred

pro = pro.estruc+pro.ma

par(mfrow=c(1,1))

plot(tp,pro.estruc, type = 'o')
lines(tp,pro, type = 'b', pch = 3,col='red' )


