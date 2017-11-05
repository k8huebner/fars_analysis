perc_cis <- function(x, n) {
  
  prob <- (x / n)
  SE <- sqrt((prob*(1-prob)/n))
  CI_lower <- prob + 1.96 * (SE*prob)
  CI_upper <- prob - 1.96 * (SE*prob)
  
  CI <- c(CI_lower, CI_upper)
  CI
}


perc_cis(x = 9000, n = 23000)