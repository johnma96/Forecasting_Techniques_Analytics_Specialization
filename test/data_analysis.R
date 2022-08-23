e = c(42, 24, 24,52)

str(e)

r = complex(3)

r[1] = -0.8 -1.2i
r[2] = Conj(r[1])
r[3] = 3

str(r)

e = as.integer(e)
str(e)


e = c(34, 314, 34 ,56)
g = c("h", "m", "m", "m")
s = c(3.4, 23.4, 3.34,34.4)
D = data.frame(edad=e, genero=g, salario=s)

str(D)


library(tidyverse)

G = tibble(
    x = 1:5,
    y = 1,
    z = x^2 + y

)

str(G)

G = as_tibble(G)

G = tibble(
    x = rnorm(5,2,3),
    y = 2,
    z = x^2 +y
)

G = as_tibble(G %>%mutate(t = time(x), u = x/t))
View(G)

Dt <- as_tibble(
D %>% mutate(edad = c(rep(23,3),rep(45,1)))
)