# Ejemplo de tipos de vectores

e = c(34,35,56,78)
g = c("h","m","m","m")
s = c(3.4,4.5,2.4,12.3)
L = c(TRUE,FALSE,FALSE,NA)

r = complex(3)
    r[1] = -0.8 - 1.3i
	r[2] = Conj(r[1])
    r[3] =3
#------

str(e)
str(g)
str(s)
str(L)
str(r)

#------
e = as.integer(e)
str(e)

a = as.numeric(L)
a
str(a)

#------
c(1, c(2, c(3, 4)))
c(1, 2, 3, 4)

#-----

A = cbind(e,s,a)
(A)

B = rbind(e,s,a)
(B)

V = B%*%A
(V)


A1 = A
for(i in 1:nrow(A)){
for(j in 1:ncol(A)){
A1[i,j] = ifelse(!is.na(A[i,j]),A[i,j],1)}}

(A1)



