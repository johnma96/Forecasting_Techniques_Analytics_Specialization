Load and Analyze Live Data from the CoViD-19 Pandemic

require(covid19.analytics)

# read data for confirmed cases
data <- covid19.data("ts-confirmed")

# compute changes and growth rates per 
location for all the countries

growth.rate(data)

#  "Growth Rates" defined as the ratio 
between changes in consecutive days
one reporting changes on daily basis 
and a second one reporting growth rates


Bvn=growth.rate(data,geo.loc="Vietnam")
Xvn = as.numeric(Bvn$Growth.Rate)
Yvn = as.numeric(Bvn$Changes)


Bnz=growth.rate(data,geo.loc="New Zealand")
Xnz = as.numeric(Bnz$Growth.Rate)
Ynz = as.numeric(Bnz$Changes)

Bco=growth.rate(data,geo.loc="Colombia")
Xco = as.numeric(Bco$Growth.Rate)
Yco = as.numeric(Bco$Changes)


Bbr=growth.rate(data,geo.loc="Brazil")
Xbr = as.numeric(Bbr$Growth.Rate)
Ybr = as.numeric(Bbr$Changes)


par(mfrow=c(2,2))

ts.plot(Xvn)
ts.plot(Xnz)

ts.plot(Xco)
ts.plot(Xbr)

par(mfrow=c(2,2))

ts.plot(Yvn)
ts.plot(Ynz)

ts.plot(Yco)
ts.plot(Ybr)


par(mfrow=c(1,1))

plot(time(Yco),Yco,type='l')
lines(time(Yvn),Yvn,col='red',lwd=2)
lines(time(Yvn),Ynz,col='blue',lwd=2)


par(mfrow=c(1,1))

plot(time(Ybr),Ybr,type='l')
lines(time(Yco),Yco,col='red',lwd=2)



