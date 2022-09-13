help("mean")

# Command to show number of packages so far
nrow(available.packages())

# Install forecast packages
install.packages("forecast", dependencies = TRUE)

#--------------------#
# 2.2 Data structure #
#--------------------#

#---------------#
# 2.2.1 Vectors #
#---------------#

vector_int <- c(34, 45, 23, 53)
vector_string <- c("f", "m", "f", "m")
vector_floats <- c(4.4, 5.6, 2.234, 3.45)
vector_boolean <- c(TRUE, FALSE, FALSE, TRUE)
vector_complex <- complex(3)
vector_complex[1] <- -0.8 - 1.3i
vector_complex[2] <- Conj(vector_complex[1])
vector_complex[3] <- 3

# str for display objects by console
str(vector_complex)


# Cast vector int
str(vector_int)

vector_int <- as.integer(vector_int)
str(vector_int)

#-------------------------#
# 2.2.2 data.frame object #
#-------------------------#

# Using vectors_int like age, vectors_string like gender and
# vector_floats like salary

dataframe_object <- data.frame(
    age = vector_int,
    gender = vector_string,
    salary = vector_floats
)
str(dataframe_object)

# You can operate over columns(variables)
es <- dataframe_object$age / dataframe_object$salary
str(es)

#------------------------------------#
# 2.2.3 Tibble and Tidyverse objects #
#------------------------------------#

# Load library tidyverse
library(tidyverse)
tibble_object <- tibble(
    x = 1.6,
    y = 1,
    z = x^2 + y
)

str(tibble_object)

# or can cast other objects
str(as_tibble(tibble_object))

str(as_tibble(dataframe_object))

# Some commands to modify a tibble object: summarize, group_by, ungroup, mutate,
# filter, as_data.frame

# For instance: Mutate

tibble_2 <- tibble(
    x = rnorm(5, 2, 3),
    y = 1,
    z = x^2 + 1
)


# %>% is called the forward pipe operator in R. It provides a mechanism for
# chaining commands with a new forward-pipe operator, %>%. This operator will
# forward a value, or the result of an expression, into the next function
# call/expression.

tibble_3 <- as_tibble(tibble_2 %>% mutate(t = time(x), u = x / t))
View(tibble_3)


# Example with data.frame

# Cast to tibble object
dataframe_object_tibble <- as_tibble(
    dataframe_object %>% mutate(age = c(23,52,22, 32))
)

View(dataframe_object_tibble)


# group data
 
mt = as_tibble(
    dataframe_object_tibble %>%
    group_by(age) %>%
    summarize(
        m = mean(salary),
        s = sd(salary),
        n = n() #Coutn number of rows in each group
    ) %>%
    ungroup() %>%
    mutate(cv = s/m)
)

View(mt)

# filter fucntion

bt = as_tibble(
    dataframe_object_tibble %>%
    filter(gender == "f", salary >= 4)
)

View(bt)

#-------------------#
# 2.2.4 List object #
#-------------------#

# List is more general than data.frames objects, are like dictionaries in 
# python or json objects (key:value)
# Lists can store othe type of objects like scalars, data.frames, strings, etc.

a = matrix(c(2,3,4,5), 2, 2)
b = c("complex", "real")
d = runif(120) # Random number between 0 and 1
list_ = list(a=a, b=b, d=d, D = dataframe_object)

str(list_)

# Extract some element
list_$D$age
