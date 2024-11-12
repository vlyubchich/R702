# My problematic function

CumSumLogs = function(x, 
                      plotit = c("base", "ggplot2", "none"),
                      plotmean = TRUE
                      ) 
{
  plotit = tolower(plotit)
  plotchoice = match.arg(plotit)
  #print(plotchoice)
  # x is a numeric vector
  # 2) better take abs values
  x = abs(x)
  y = log(x)
  ## 1) patching the code
  #if (any(is.na(y))) {
  #  print("replace NAs!")
  #  y[is.na(y)] = 0
  #}
  z = cumsum(y)
  
  if (plotchoice == "base") {
    plot(z)
    if (plotmean) {
      abline(h = mean(z))
    }
  } else if (plotchoice == "ggplot2") {
    require(ggplot2)
    p <- ggplot2::ggplot(data.frame(index = 1:length(z),
                               z = z),
                    aes(x = index, y = z)) + 
      geom_point()
    if (plotmean) {
      p = p + geom_hline(yintercept = mean(z))
    }
    print(p)
  }
  
  z
}

x = 1:200
CumSumLogs(x)
CumSumLogs(x, plotmean = FALSE)
CumSumLogs(x, "gg")
CumSumLogs(x, "gg", plotmean = FALSE)
CumSumLogs(x, "GG")
CumSumLogs(x, "none")

d = CumSumLogs(x, "none")













set.seed(123)
x = rnorm(100, mean = 2)
CumSumLogs(x)

is.na(log(x))
x[72]
x[70:75]


D = iris
x = D[, 1]
is.vector(x)
is.vector(D)
is.data.frame(x)

# Do not drop dimensions
x = D[, 1, drop = FALSE]
is.vector(x)
is.data.frame(x)



