# https://es.r4ds.hadley.nz/tibbles.html#tibbles-vs.-data.frame
# ejemplo simple 
library(tidyverse)
#--------------crear un tibble
G = tibble(
  x = rnorm(5,2,3),
  y = 1,
  z = x^2 + y
)

G = as_tibble(G %>%mutate(t = time(x), u = x/t))

View(G)

#-------convierte tibble a data.frame
D = as.data.frame(G)
str(D)