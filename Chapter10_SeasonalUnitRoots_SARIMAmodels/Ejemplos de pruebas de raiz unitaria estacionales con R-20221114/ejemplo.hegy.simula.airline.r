#----- Ejemplo Notas simular el modelo airline
ti=-0.377; ti12=-0.572; 
sigma= sqrt(0.0014)

library(CombMSC)

y = sarima.Sim(n = 20, period =12, 
model=list(order=c(0,1,1),ma = ti,
ar = NULL, sd = sigma), 
seasonal=list(order=c(0,1,1),ma=ti12,ar = NULL),
rand.Gen.Fun = rnorm, rand.Gen.Seas = rnorm)


require(uroot)

res = hegy.test(y, 
deterministic = c(1,0,0),
lag.method = "AIC", 
maxlag = 0,
pvalue =  "raw", 
rs.nobsreg = 15, 
boot.args = list(seed = 123, 
lag.method = lag.method[1], maxlag = maxlag,
byseason = FALSE, nb = 1000, 
BTdim = c(100, 10), debug.tid = -1))

res
