# simular modelo con nivel y tendencia local ##

sim.nivel.tend.local <- function(a1,a2,n){
 bt = double(n)
 ut = double(n)
 yt = double(n)
 et = rnorm(n,0,1)
 for (i in 2:n) {
 bt[i] = bt[i-1]+a2*et[i]
 ut[i] = ut[i-1]+bt[i-1]+a1*et[i]
 yt[i] = ut[i-1]+bt[i-1]+et[i]
 }
 return(yt)
 }

a1 = 0.004
 a2 = 0.02
n = 300
yt = sim.nivel.tend.local(a1,a2,n)

ts.plot(yt)

# library(MASS)
# write.matrix(yt,"LLTMmodelo.dat")