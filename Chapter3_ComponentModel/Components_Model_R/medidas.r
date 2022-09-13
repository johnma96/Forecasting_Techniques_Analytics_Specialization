medidas = function(m,y,k){
# m = objeto producido con lm()
# y = variable dependiente
# k = n�mero de coeficientes beta
T = length(y)
yest = fitted(m)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
mse = sse/(T-k)
R2 = 1 - sse/ssr
Ra2 = 1 - (T-1)*(1-R2)/(T-k)
aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
M = c(sqrt(mse),Ra2,  aic, bic)
names(M) = c("rmse","R2-ad","log.aic","log.bic")
return(M)
}