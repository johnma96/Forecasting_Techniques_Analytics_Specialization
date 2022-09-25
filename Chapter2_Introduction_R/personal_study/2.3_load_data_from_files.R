#---------------------------#
# 2.3.1 read.table function #
#---------------------------#

# Return data.frame object. It's possible use prn extensions to read excel f
# iles, just save one copy of the excel with this extension

# Read from an URL

url <- "http://robjhyndman.com/tsdldata/data/nybirths.dat"
g <- read.table(url)

View(g)

x <- ts(g,
    frequency = 12,
    start = c(1946, 1)
) # Use ts to create time-series objects, assing to each value a date, this mean
# that we have a monthly series from 1946

str(x)


# grafica
np <- length(x)

fechas <- seq(as.Date("1946/01/01"), as.Date("1959/12/31"), by = "months")
ejex.month <- seq(fechas[1], fechas[np], "months")
ejex.year <- seq(fechas[1], fechas[np], "years")

plot(fechas, x,
    xaxt = "n", panel.first = grid(),
    type = "b", ylab = "Número recien nacidos"
)
axis.Date(1, at = ejex.month, format = "%m/%y")
axis.Date(1, at = ejex.year, labels = FALSE, txl = -0.2)

#-------------------#
# 2.3.3 ts function #
#-------------------#

# Read time-series like numeric vector
y <- structure(c(
    1574, 1368, 1387, 1109, 1257, 1376, 2143, 1208,
    2007, 1876, 1702, 1819, 1802, 1205, 1684, 1682, 1991, 2394, 1914,
    2499, 2130, 2529, 2328, 2076, 2496, 1647, 2518, 2205, 2395, 2891,
    2712, 2427, 2477, 2860, 2505, 3355, 1760, 2318, 3111, 2570, 2868,
    3042, 2749, 2839, 3140, 2909, 2982, 3667, 2814, 2732, 3265, 3166,
    2792, 3742, 3099, 3278, 4120, 3553, 3675, 3799, 3427, 3234, 3733,
    3642, 3553, 3647, 3624, 2973, 3597, 3731, 4092, 4100, 2762, 3953,
    4152, 4229, 4419, 4774, 4313, 4060, 4664, 4374, 4419, 4908, 4321,
    4772, 4361, 4969, 5111, 5014, 4858, 5159, 5086, 5379, 5605, 5269
))

# Convert to ts object
y <- ts(y, frequency = 12, start = c(1990, 1))

# Generate date's vector with Date class
dates <- seq(as.Date("1990/3/1"), length.out = length(y), by = "months")

# Graph
ts.plot(y, main = "Serie F")

# More details
np <- length(y)
ejex.month <- seq(dates[1], dates[np], "months")
ejex.year <- seq(dates[1], dates[np], "years")

plot(dates, y,
    xaxt = "n", panel.first = grid(),
    type = "b", ylab = "producción por mes"
)

axis.Date(1, at = ejex.month, format = "%m/%y")
axis.Date(1, at = ejex.year, labels = FALSE, tcl = -0.2)

# Horizontal line at 4000 level
abline(h = 4000, col = "red")

# What's date on wich 4000 level is exceeded for the first time

j <- 1

while (y[j] < 4000) {
    j <- j + 1
}

# Answer
str[dates[j]]

# Add vertical line on that date

points(dates[j], y[j], type = "h", col = "blue")

#------------------------------------#
# 2.3.4 Excel file reading functions #
#------------------------------------#

install.packages("readxl")
library(readxl)

res <- read_excel("Chapter2_Introduction_R/Basic_Series_R/international-petroleum-world-cr.xlsx")
attach(res)

plot.ts(res[, c(2, 3, 4, 5)])
