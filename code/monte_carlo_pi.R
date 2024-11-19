monte_carlo_pi <- function(n) {
  # Generate random points within a square of side length 2
  x <- runif(n, min = -1, max = 1)
  y <- runif(n, min = -1, max = 1)
  
  # Calculate the number of points within the unit circle
  inside_circle <- sum(x^2 + y^2 <= 1)
  
  # Estimate Pi
  pi_estimate <- 4 * inside_circle / n
  
  return(pi_estimate)
}



monte_carlo_pi2 <- function(n, cluster = NULL) {
  # cluster is the number of cores the user wants to use
  if (!is.null(cluster) & cluster > 1) {
    cl <- parallel::makeCluster(cluster)
  }
  
  inside_circle <- parallel::parSapply(cl, 1:n, function(i) {
    # Generate random points within a square of side length 2
    x <- runif(1, min = -1, max = 1)
    y <- runif(1, min = -1, max = 1)
    
    # Calculate the number of points within the unit circle
    (x^2 + y^2) <= 1
  })
  
  # Estimate Pi
  pi_estimate <- 4 * sum(inside_circle) / n
  
  if (!is.null(cluster)) {
    parallel::stopCluster(cl)
  }
  
  return(pi_estimate)
}
