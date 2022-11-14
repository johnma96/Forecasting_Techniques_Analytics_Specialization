# ANALISIS CA daily  R. Weron

E = read.table("CA_daily.dat", header = TRUE,
stringsAsFactors=FALSE)
attach(E)
   
y = log(na.omit(PRICE))
# generar un vector de fechas, clase 'Date' 

#-------------- validacion cruzada
T = length(y)
m = 30
yf = y[(T-m+1):T]
yi = ts(y[1:(T-m)],frequency=7)
t = seq(1,length(yi))

par(mfrow=c(1,1))
fechas = seq(as.Date("1998/4/1"), length.out = length(yi), by =  "days")
np = length(yi)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,yi, xaxt="n", panel.first = grid(),type='l',col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)


#--------------------
mod1 = loess(yi ~ t, span=0.75,
control = loess.control(surface = "direct"))

#--------------------

ys = yi-mod1$fitted

plot(fechas,ys, xaxt="n", panel.first = grid(),type='l',col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)


#--------------------

require(forecast)

ys = ts(ys,frequency=360) 
It = fourier(ys,2)

ys = ts(ys,frequency=7) 
It7 = fourier(ys,2)

mod2 = lm(ys ~ t +  It7 + It)
summary(mod2)

ys.hat = fitted(mod2)

lines(fechas,ys.hat,col='red')


at = resid(mod2) 

par(mfrow=c(3,2))
plot(t,at,type='l',ylab='residuo')
abline(h=0,lty=2)
plot(density(at),xlab='x',main= '')
acf(at,60,ci.type="ma",main="")
pacf(at,60,main="")
qqnorm(at)
qqline(at,col=2)
cpgram(at)
#--------------------residuos estructurales

k99 = kernel("modified.daniell", c(9,9))

ye = ts(resid(mod2),frequency=7)

par(mfrow=c(2,2))

ts.plot(ye)
acf(ye,90)
pacf(ye,90)
spec.pgram(ye, k99, taper=0,  log = "dB", ci = 0.8)
abline(v=1,col='red')
abline(v=2,col='blue')
abline(v=3,col='blue')

#------------identificador auto.arima

auto.arima(ye)
ARIMA(3,0,2)(1,0,1)[7] with zero mean 



#------------identificador identify
require(aTSA)
identify(ye,p=4,q=4,stat.test = TRUE)
Minimum Information Criterion: 
        MA0     MA1     MA2     MA3     MA4
AR0  884.93  198.79 -107.34 -192.85 -278.66
AR1 -333.93 -335.01 -333.09 -337.33 -336.72
AR2 -335.03 -333.02 -332.87 -341.57 -339.58
AR3 -333.03 -332.28 -342.63 -340.03 -338.17
AR4 -336.22 -342.40 -340.76 -339.15 -339.23

Minimum Table Value: AICC(3,2)  =  -342.6263 

#------------

mod3 = arima(ye,
order=c(3,0,2),
seasonal = list(
order = c(1, 0, 1), 
period = 7),include.mean = FALSE)

require(lmtest)
coeftest(mod3)


at = resid(mod3) 

require(TSA)
par(mfrow=c(3,2))
plot(t,at,type='l',ylab='residuo')
abline(h=0,lty=2)
plot(density(at),xlab='x',main= '')
acf(at,60,ci.type="ma",drop.lag.0=TRUE,main="")
pacf(at,60,main="")
qqnorm(at)
qqline(at,col=2)
cpgram(at)


Box.test(x = at, lag = 21, type="Ljung-Box")


#----------------pronosticos
 
tf = seq((T-m+1),T)
pr.1 = predict(mod1, data.frame(t = tf))

ys = ts(ys,frequency=360) 
Itf = fourier(ys,2,m)

ys = ts(ys,frequency=7) 
Itf7 = fourier(ys,2,m)

pr.2 = predict(mod2,
data.frame(t = tf,It=I(Itf),It7=I(Itf7)))

pr.3 = predict(mod3,n.ahead=m)$pred

sde = predict(mod3, n.ahead=m)$se

py = pr.1+pr.2+pr.3

T0 = 60
b = c(yi[(T-m-T0):(T-m)],yf)
T1 = length(b)

par(mfrow=c(1,1))
plot(seq((T-m-T0),T),b, 
type='l', col=1)
lines(tf,py, col='red',lwd=2)

lines(tf, py+1.64*sde,  col='magenta')
lines(tf, py-1.64*sde,  col='magenta')


legend("topright", 
c("obs","loess+fourier+sarma"), 
lty = c(1,1),
lwd=c(2,2),
col=c('red','blue') )


# compara mape
yf = ts(yf,frequency=7)
p0 = ts(pr.1+pr.2,frequency=7)
p1 = ts(py,frequency=7)


A=rbind(
accuracy(yf,p0), 
accuracy(yf,p1))
(A)

(cbind(yf,p0,p1))



