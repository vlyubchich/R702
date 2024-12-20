TD <- function() {
    x = Sys.Date()
    Year = as.numeric(format(x, "%Y"))
    Month = as.numeric(format(x, "%m"))
    MonthName = format(x, "%B")
    Day = as.numeric(format(x, "%d"))
    print(paste("Today is", MonthName, Day))
    list(Year = Year, Month = Month, Day = Day)
}

TDx <- function(dates = NULL) {
    if (is.null(dates)) {
        x = Sys.Date()
    } else {
        x = dates
    }
    Year = as.numeric(format(x, "%Y"))
    Month = as.numeric(format(x, "%m"))
    # MonthName = format(x, "%B")
    Day = as.numeric(format(x, "%d"))
    # print(paste("Today is", MonthName, Day))
    data.frame(Year = Year, Month = Month, Day = Day)
}

myplot <- function(x, y,
                   las = 1, pch = 15, col = "tomato",
                   ...) {
  plot(x, y, 
       las = las, pch = pch, col = col,
       ...
  )
}
  
mybadplot <- function(x, y,
                   ...) {
  plot(x, y, 
       las = 1, pch = 15, col = "tomato",
       ...
  )
}

isleap <- function(year){
  if (year %% 4 == 0) {
    if (year %% 100 == 0) {
      if (year %% 400 == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

# isleap(2000)
# isleap(2024)
# isleap(2023)
# isleap(2300)
# 
# isleap(2020:2024)
# year = 2020:2024
# 
# sapply(2020:2024, isleap)


isleap <- function(year){
  ch1 = (year %% 4 == 0) 
  ch2 = (year %% 100 == 0)
  ch3 = (year %% 400 == 0)
  ch1 & ch2
}

# isleap(2000)
# isleap(2020:2024)


