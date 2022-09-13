sim.ntys.local <- function(a1,a2,a3,n,m,S0){
# modelo con nivel,tendencia y estacionalidad local
st<-double(n)
st[1:m] = S0
bt<-double(n)
ut<-double(n)
et<-double(n)
yt<-double(n)
et<-rnorm(n,0,1)
v1t<-rnorm(n,0,1)
v2t<-rnorm(n,0,1)
v3t<-rnorm(n,0,1)
for (i in (m+1):n){
st[i]<-st[i-m]+a3*et[i]
bt[i]<-bt[i-1]+a2*(et[i])
ut[i]<-ut[i-1]+bt[i-1]+a1*et[i]
yt[i]<-ut[i-1]+st[i-m]+bt[i-1]+et[i]
}
D = cbind(yt,ut,bt,st)
return(D)
}

a1 = 0.01
a2 = 0.003
a3 = 0.5
n = 12*20
m = 12

S0 = c( -0.591612582, -0.197615742,  0.253540194, -0.006133961,  1.767530937,
0.164159153,  0.247873206, -0.705350538, -0.052363843,  0.041315912,
0.114998587, -0.417584266)

D = sim.ntys.local(a1,a2,a3,n,m,S0)

plot.ts(D)