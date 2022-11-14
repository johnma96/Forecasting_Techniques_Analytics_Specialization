#https://fred.stlouisfed.org/series/ODCNPI03DEQ156N?utm_source=series_page&utm_medium=related_content&utm_term=other_formats&utm_campaign=other_format
#Categories > International Data > Countries > Germany

D = read.csv("residentialbuildingpermitsgermany.fred.csv", header=TRUE)
y = ts(D$ODCNPI03DEQ156N,frequency=4,start=c(1979,01))
ts.plot(y)

#---------modelo sarima
require(forecast)
auto.arima(y)
ARIMA(1,0,0)(0,1,1)[4] 

#--------validacion cruzada
T = length(y)
m = 16
yi = ts(y[1:(T-m)],frequency=4)
yf = ts(y[(T-m+1):T],frequency=4)


#------estimacion
mod1 = arima(yi,order=c(1,0,0),
seasonal=list(order=c(0,1,1),period = 4))

require(lmtest)
coeftest(mod1)

yhat = yi- mod1$residuals

t = seq(1,length(yi))

par(mfrow=c(1,1))
plot(t,yi,type='o')
lines(t,yhat,col='red')

#--------valida residuos

e = mod1$resid

library(TSA)
par(mfrow=c(3,2))
plot(t,e,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(e),xlab='x',main= '')
acf(e,30,ci.type="ma",main="")
pacf(e,30,main="")
qqnorm(e)
qqline(e,col=2)

Box.test(x = e, lag = 28, type="Ljung-Box")

#---------pronosticos
pron = predict(mod1,n.ahead=m)
sde = pron$se
p1 = pron$pred

T0 = 40
b = c(yi[(T-m-T0):(T-m)],yf)
T1 = length(b)
tt = seq((T-m+1),T)

par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='b', ylim=c(1000,65000))
lines(tt,p1, col='blue')

lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","sarima"), 
lty = c(1,1),
lwd=c(2,2),
col=c('black','blue') )

#--------------modelo componentes


It = seasonaldummy(yi)

yw = loess(yi ~ t, span=0.5,
control = loess.control(surface = "direct"))

yest.yw = yw$fitted

Si = yi - yest.yw

mod2 = lm(Si ~ It)
summary(mod2)

e = ts(mod2$resid,frequency=4)

library(TSA)
par(mfrow=c(3,2))
plot(t,e,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(e),xlab='x',main= '')
acf(e,30,ci.type="ma",main="")
pacf(e,30,main="")
qqnorm(e)
qqline(e,col=2)

Box.test(x = e, lag = 28, type="Ljung-Box")

auto.arima(e)
ARIMA(1,0,0)(2,0,0)[4] with zero mean

mod3 = arima(e,order=c(1,0,0),
seasonal=list(order=c(2,0,0),period=4))

e3=mod3$resid

Box.test(x = e3, lag = 28, type="Ljung-Box")


#----------pronosticos

pr.yw = predict(yw, data.frame(t = tt))

Itt = seasonaldummy(yi,m)

pr.st = predict(mod2,data.frame(It=I(Itt)))

pr.sarma = predict(mod3,n.ahead=m)$pred

pr.y = pr.yw+pr.st+pr.sarma



par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='b', ylim=c(6000,65000))
lines(tt,p1, col='blue')
lines(tt,pr.y, col='red')
lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","sarima","loess-ind-sarma"), 
lty = c(1,1,1),
lwd=c(2,2,2),
col=c('black','blue','red') )


#-----------------------aplicar Holt-Winters

y.hw = HoltWinters(yi)
(c(y.hw$alpha,y.hw$beta,y.hw$gamma))
# la tendencia, componente estacional y y estimada

yhat.hw = fitted(y.hw)[,1]


par(mfrow=c(1,1))
plot(t,yi,type='o')
lines(t[-seq(1,4)],yhat.hw,col='red')


# y  pronosticos se calculan como
pr.hw = predict(y.hw,m) # prediction.interval = TRUE)


par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='b', ylim=c(6000,65000))
lines(tt,p1, col='blue')
lines(tt,pr.y, col='red')
lines(tt,pr.yw, col='brown')
lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","sarima","loess-ind-sarma","HW"), 
lty = c(1,1,1,1),
lwd=c(2,2,2,2),
col=c('black','blue','red','brown') )

#---------------------------aplicar TS-Struct

y.bsm = StructTS(yi)
print(y.bsm)
str(y.bsm)
fitted(y.bsm)
# "level" "slope" "sea"

yhat.bsm = apply(fitted(y.bsm),1,sum)

par(mfrow=c(1,1))
plot(t,yi,type='o')
lines(t,yhat.bsm,col='red')

require(forecast)
B=forecast::forecast(y.bsm,h=m)
pr.bsm = B$mean
pr.bsm = ts(B$mean,frequency=4)


par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='b', ylim=c(6000,65000))
lines(tt,p1, col='blue')
lines(tt,pr.y, col='red')
lines(tt,pr.yw, col='brown')
lines(tt,pr.bsm, col='darkgreen',lwd=2)
lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","sarima","loess-ind-sarma","HW","StructTS"), 
lty = c(1,1,1,1,1),
lwd=c(2,2,2,2,2),
col=c('black','blue','red','brown','green') )



#-----------------aplicar red neuronal

y.nnar = nnetar(yi,lambda=0)

# y.nnar = nnetar(yi,p=2,P=1,size=3,lambda=0)

print(y.nnar)

yhat.nnar= fitted(y.nnar)


par(mfrow=c(1,1))
plot(t,yi,type='o')
lines(t,yhat.nnar,col='red')


pr.nnar = forecast::forecast(y.nnar,h=m)$mean
sde = ts(sde,frequency=4)

par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='b', ylim=c(6000,65000))
lines(tt,p1, col='blue')
lines(tt,pr.y, col='red')
lines(tt,pr.yw, col='brown')
lines(tt,pr.bsm, col='darkgreen',lwd=2)
lines(tt,pr.nnar, col='magenta',lwd=2)
lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","sarima","loess-ind-sarma","HW","StructTS","NNAR"), 
lty = c(1,1,1,1,1,1),
lwd=c(2,2,2,2,2,2),
col=c('black','blue','red','brown','darkgreen','magenta') )


#--------------compara calidad pronosticos
pr.y = ts(pr.bsm,frequency=4)
pr.hw = ts(pr.hw,frequency=4)
pr.nnar = ts(pr.nnar,frequency=4)
p1 = ts(p1,frequency=4)
pr.yw = ts(pr.yw,frequency=4)

M = rbind(accuracy(p1,yf),
accuracy(pr.yw,yf),
accuracy(pr.hw,yf),
accuracy(pr.bsm,yf),
accuracy(pr.nnar,yf))
rownames(M) = c("sarima","loess-ind-sarma","HW","StructTS","NNAR")
(M)





