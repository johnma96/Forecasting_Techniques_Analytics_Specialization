# --------ejemplo simple de objeto list
#         usando el data.frame D del ejemplo: ''ejemplo.media.por.grupos.r''
a = matrix(c(2,3,4,5),2,2)
b = c("complex","real")
d = runif(120) # 120 números aleatorios entre 0 y 1
L  = list(a=a,b=b,d=d,D=D)

str(L)
$ a: num [1:2, 1:2] 2 3 4 5
 $ b: chr [1:2] "complex" "real"
 $ d: num [1:120] 0.04572 0.00316 0.82478 0.83271 0.99105 ...
 $ D:'data.frame':      9 obs. of  4 variables:
  ..$ Nombre: chr [1:9] "Aira" "Aira" "Aira" "Ben" ...
  ..$ Mes   : int [1:9] 1 2 3 1 2 3 1 2 3
  ..$ Rate1 : int [1:9] 12 18 19 53 22 19 22 67 45
  ..$ Rate2 : int [1:9] 23 73 45 19 87 45 87 43 32

L$D$Nombre
[1] "Aira" "Aira" "Aira" "Ben"  "Ben"  "Ben"  "Cat"  "Cat"  "Cat" 
