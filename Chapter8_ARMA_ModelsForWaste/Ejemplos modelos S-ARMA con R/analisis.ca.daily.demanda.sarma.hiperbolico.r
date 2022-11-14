# ANALISIS CA daily  R. Weron

E = read.table("CA_daily.dat", header = TRUE)
attach(E)
   
y = LOAD
# generar un vector de fechas, clase 'Date' 
fechas = seq(as.Date("1998/4/1"), length.out = length(y), by =  "days")

par(mfrow=c(1,1))


np = length(y)

ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")


plot(fechas,y, xaxt="n", panel.first = grid(),type='l',col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#-------------- validacion cruzada
T = length(y)
m = 30
yf = y[(T-m+1):T]
yi = y[1:(T-m)]

require(forecast)

ys = ts(yi,frequency=360) 
It = fourier(ys,2)
t = seq(1,length(ys))
t2 = t*t
y7 = ts(yi,frequency=7) 
It7 = fourier(y7,K=3)


M1 = lm(ys ~ t + t2 + It+It7)
summary(M1)

y1 = fitted(M1)

np = length(ys)
fechas = fechas[1:np]
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")


plot(fechas,ys, xaxt="n", panel.first = grid(),type='l',col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fechas,y1,col='red')

ye = resid(M1)

plot(fechas,ye, xaxt="n", panel.first = grid(),
type='l',col='black')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#--------------------
ye = ts(ye,frequency=7)

#------------identificador armasubsets
require(TSA)
res=armasubsets(y=ye,
nar=14,nma=14,
y.name='y',
ar.method='ols')

par(mfrow=c(1,1))
plot(res)

ARIMA(2,0,0)(1,0,0)[7] with zero mean


#------------identificador auto.arima

auto.arima(ye)
ARIMA(1,0,2)(0,0,1)[7] with zero mean

#------------identificador identify
require(aTSA)
identify(ye,p=4,q=4,stat.test = TRUE)
Minimum Information Criterion: 
      MA0   MA1   MA2   MA3   MA4
AR0 29842 28536 28039 27832 27768
AR1 27809 27723 27716 27718 27707
AR2 27716 27718 27718 27717 27701
AR3 27718 27720 27701 27718 27720
AR4 27712 27700 27716 27704 27704

Minimum Table Value: AICC(4,1)  =  27700.39 


#------------armasubsets

mod1 = arima(ye,
order=c(2,0,0),
seasonal = list(
order = c(1, 0, 0), 
period = 7),include.mean = FALSE)

#------------auto.arima

mod2 = arima(ye,
order=c(1,0,2),
seasonal = list(
order = c(0, 0, 1), 
period = 7),include.mean = FALSE)


#------------identify

mod3 = arima(ye,
order=c(4,0,1),include.mean = FALSE)



(c(AIC(mod1),AIC(mod2),AIC(mod3)))


at = resid(mod1) 

par(mfrow=c(3,2))
plot(t,at,type='l',ylab='residuo')
abline(h=0,lty=2)
plot(density(at),xlab='x',main= '')
acf(at,60,ci.type="ma",main="")
pacf(at,60,main="")
qqnorm(at)
qqline(at,col=2)
cpgram(at)


Box.test(x = at, lag = 21, type="Ljung-Box")



#----------------pronosticos

 
Itts = fourier(ys,2,m)
tt = seq((T-m+1),T)
tt2 = tt*tt
Itt7 = fourier(y7,3,m)



p0 = predict(M1,data.frame(t = tt,t2=tt2,
It=I(Itts),It7=I(Itt7)))

p1 = p0+predict(mod1,n.ahead=m)$pred

sde = predict(mod1, n.ahead=m)$se

T0 = 60
b = c(yi[(T-m-T0):(T-m)],yf)
T1 = length(b)

par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='l', col=2,ylim=c(20000,35000))
lines(tt,p1, col='blue')

lines(tt, p1+1.64*sde,  col='magenta')
lines(tt, p1-1.64*sde,  col='magenta')


legend("topright", 
c("obs","cuad+fourier+sarma"), 
lty = c(1,1),
lwd=c(2,2),
col=c('red','blue') )


# compara mape
yf = ts(yf,frequency=7)
p0 = ts(p0,frequency=7)
p1 = ts(p1,frequency=7)


A=rbind(
accuracy(yf,p0), 
accuracy(yf,p1))
(A)

(cbind(yf,p0,p1))

#----------------residuos hiperbólicos?
?dhyp

require(fBasics)

V=hypFit(at, alpha = 10, beta = 0.1, delta = 0.1, 
mu = mean(at), doplot = TRUE) 

coef = V@fit$estimate
# Histograma:

hist(at, col = "steelblue", probability = TRUE, 
breaks = 100, border = "white", 
xlim = c(min(at), max(at)))
abline (h = 0)
x = seq(min(at), max(at), length = 201)

lines(x, dnorm(x = x, mean = mean(at), 
sd = sqrt(var(at))),lwd=2)


lines(x, dhyp(x = x, alpha = coef[1], 
beta = coef[2],delta = coef[3],mu = coef[4])
,lwd=2,col='red')




