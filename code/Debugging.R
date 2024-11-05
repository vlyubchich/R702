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






#### TOOLS
traceback()

mean(x)
traceback()

# browser() and debug()
source('MyFunctions.R')


debug()
