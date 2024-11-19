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
