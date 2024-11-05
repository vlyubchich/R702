#####################################################################
#####################################################################
library(tidyverse)
message("Hello there")

# Debugging
Dates = as.Date(c("2011-04-05", 35))
min(Dates)
mod = lm(1:2 ~ Dates)
summary(mod)

# Warning
tmp <- log(-pi)
is.na(tmp)

B = 500 # boot replications
1/B
0.05

log(pi)

# Error
lm(x ~ y, weights = z, iris, col = "blue") #we do not have x and y in our environment

tapply(iris$Sepal.Length, 
       iris$Species, 
       quantile,
       probs = c(0.5), 
       na.rm = TRUE)

# Functions ---- 
lm

Sys.Date()

# Function to report Year, Month, and Day of today
source("./code/fun_todaydate.R")
# source("fun_todaydate.R")

TD()

TDx()
TDx(as.Date("2020-12-31"))
TDx(as.Date(c("2020-12-31", "2028-10-16")))

TDx(seq.Date(from = as.Date("2020-12-31"), 
             to = as.Date("2021-12-31"), by = 5))


# Nice plot

iris
x = iris$Sepal.Length
y = iris$Petal.Length

plot(x, y)

plot(x, y, 
     las = 1, pch = 15, col = "tomato", cex = 2)


source("code/fun_todaydate.R")

myplot(iris$Sepal.Length, iris$Petal.Length)
myplot(iris$Sepal.Length, iris$Petal.Length, pch = 5)
myplot(iris$Sepal.Length, iris$Petal.Length, pch = 5, col = "blue", cex = 2)

mybadplot(iris$Sepal.Length, iris$Petal.Length)
mybadplot(iris$Sepal.Length, iris$Petal.Length, pch = 5, col = "blue", cex = 2)


#### TOOLS
traceback()

mean(x)
traceback()

# browser() and debug()
source('MyFunctions.R')


debug()
