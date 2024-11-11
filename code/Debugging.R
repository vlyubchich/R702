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

debug()

browser()

# browser() and debug()
source('code/MyFunctions.R')

# Practice ----
## Using traceback ----
funPower(2, 3)
traceback()

## Using line-by-line execution ----
x = 2; y = 3
funPower2 <- function(x, y){
  z <- x^2
}
z = x^y
z
ra <- funRatio(x, u)

## Using debug ----
# Mark a function that will be debugged
debug(funPower)
funPower(2, 3)

# after finishing debugging
undebug(funPower)

# Can apply to other functions
debug(lm)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
undebug(lm)


## Using browser ----
# Place browser() in your code and 
# source the function again.
source('code/MyFunctions.R')
funPower(2, 3)
# after fixed errors, remove browser() and source the 
# function again.
source('code/MyFunctions.R')
funPower(2, 3)


funPower(2, y = 2)
funPower(2, 3)
