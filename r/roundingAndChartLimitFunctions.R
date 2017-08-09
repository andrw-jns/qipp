cat("roundingAndChartLimitFunctions.R")
cat("Rounding functions")
cat("roundTo(x, roundTo, roundingDirection = \"nearest\")
  rounds a number up/down/to the nearest multiple of roundTo")
roundTo <- function(x, roundTo, roundingDirection = "nearest"){
  
  ## Takes a number, rounds it to the nearest specified number, in the relevant direction
  ans <- round(as.double(x) / (roundTo)) * (roundTo)
  if(!is.na(roundingDirection)){
    if(tolower(roundingDirection) == "down"){
      if(ans > x) {
        ans <- ans - roundTo
      }
    } else if(tolower(roundingDirection) == "up") {
      if(ans < x) {
        ans <- ans + roundTo
      }
    }
  }
  return(ans)
}

cat("roundToChartLimits(x, roundTo, textBuffer = FALSE) 
  rounds the min and max of a given series for plotting purposes")
roundToChartLimits <- function(x, roundTo, textBuffer = FALSE){
  ans <- c(roundTo(min(x, na.rm = T), roundTo, "down")
           , roundTo(max(x, na.rm = T), roundTo, "up"))
  if(textBuffer){# pad the limits
    ans <- c(
      ifelse(ans[1] < 0, ans[1] - roundTo, ans[1]) #only pad if min is less than zero.
      , ans[2] + roundTo)
  }
  return(ans)
}

cat("\nFinancial Year functions for plotting")
cat("FYearIntToChar(x)
  Returns character from integer financial year (e.g. 201415 becomes 2014/15)")
FYearIntToChar <- function(x){
  # Returns character from integer financial year (e.g. 201415)
  return(paste0(substring(x, 1, 4), "/", substring(x, 5, 6)))
}

cat("FYearDateToInt(x)
  Returns integer financial year from date (e.g. 2015-01-01 becomes 201415)")
FYearDateToInt <- function(x){
  #Returns integer financial year from Date
  y <- year(x)
  ans <- 
    ifelse(
      month(x) < 4
      , paste0(y - 1, substring(y, 3, 4))
      , paste0(y, substring(y + 1, 3, 4))
    ) %>% as.integer
  return(ans)
}

cat("FYearDateToChar(x)
  Returns integer financial year from date (e.g. 2015-01-01 becomes 2014/15)")
FYearDateToChar <- function(x){
  # Returns character financial year from Date
  y <- year(x)
  ans <- 
    ifelse(
      month(x) < 4
      , paste0(y - 1, "/", substring(y, 3, 4))
      , paste0(y, "/", substring(y + 1, 3, 4))
    )
  return(ans)  
}

cat("\nPlotting functions")
cat("chartBestTick(max, mostTicks = 8)
  returns the best tick value for a series with a given max and maximum number of ticks")
chartBestTick <- function(max, mostTicks = 8){
    minimum   <- max / mostTicks
    magnitude <- 10 ^ round(log(minimum, base = 10))
    residual  <- minimum / magnitude
    if(residual > 5){tick = 10 * magnitude
    } else if(residual > 2){tick = 5 * magnitude
    } else if(residual > 1){tick = 2 * magnitude
    } else{tick = magnitude}
    return(tick)
}

cat("chartLimits(minimum, maximum, mostTicks = 8)
  returns a sensible minimum and maximum for chart limits")
chartLimits <- function(minimum, maximum, mostTicks = 8){
   range <- maximum - minimum
   exponent <- round(log(range, base = 10))
   magnitude <- 10 ^ exponent
   
   tick <- chartBestTick(maximum, mostTicks)
   adjMin <- roundTo(minimum, tick, "down")
   adjMax <- roundTo(maximum, tick, "up")
   chartLimits <- c(adjMin, adjMax)
   names(chartLimits) <- c("Min", "Max")
   return(chartLimits)
 }

cat("chartBreaks(minimum, maximum, mostTicks = 8)
  returns a sensible minimum, and maximum, and breaks for charts.")
chartBreaks <- function(minimum, maximum, mostTicks = 8){
  limits <- chartLimits(minimum, maximum)
  tick   <- chartBestTick(maximum, mostTicks)
  breaks <- seq(limits["Min"], limits["Max"], tick)
  return(breaks)
}
