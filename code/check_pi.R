rm(list = ls())
source("code/monte_carlo_pi.R")

# Sequential long operation
system.time(
    monte_carlo_pi(10000000)
)
#user  system elapsed 
# 2.58    0.21    7.05
# 2.68    0.19    7.16

system.time(
  sapply(1:10, function(i) {
    monte_carlo_pi2(10000000, parallel::detectCores())
  })
)

cl <- parallel::makeCluster(8)
parallel::clusterExport(cl,
                        varlist = c("monte_carlo_pi"),
                        envir = environment())
system.time(
  parallel::parSapply(cl, 1:10, function(i) {
    monte_carlo_pi(10000000)
  })
)
parallel::stopCluster(cl)
# user  system elapsed 
# 0.02    0.02    3.47 
