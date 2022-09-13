#---------------------------------------

E = read.table("dengue.dat", header = TRUE, stringsAsFactors = FALSE)
attach(E)

y = ts(y,frequency=52)

m1 = stl(y, s.window = "per", robust = TRUE)
plot(m1)

Tt = m1$time.series[,2]
St = m1$time.series[,1]
et = m1$time.series[,3]

t = seq(1,length(y))

plot(t,y,type='l')

library(ggplot2)
library(tidyverse)

wekseq<-seq(as.Date("2002-01-01"), by="1 week",
length.out = length(y))

dengue = data.frame(fechas=wekseq,y = y)

ggplot(dengue,aes(x=fechas,y=y)) + 
  geom_point() + 
  geom_line() +
  scale_x_yearweek(date_breaks="16 weeks", date_labels = "%U")

