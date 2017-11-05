perc_cis <- function(x, n) {
  
  prob <- (x / n)
  SE <- sqrt((prob*(1-prob)/n))
  CI_lower <- prob - (1.96 * (SE*prob))
  CI_upper <- prob + (1.96 * (SE*prob))
  
  CI <- c(CI_lower, CI_upper)
  CI
  print_out <- c(prob, CI)*100
  print_out <- round(print_out, digits = 1)
  print_out 
}


perc_cis(x = 9000, n = 23000)

