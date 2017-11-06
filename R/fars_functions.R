perc_cis <- function(x, n) {
  
  prob <- (x / n)
  SE <- sqrt((prob*(1-prob)/n))
  CI_lower <- prob - (1.96 * (SE*prob))
  CI_upper <- prob + (1.96 * (SE*prob))
  
  CI_lower_perc <- round(CI_lower*100, digits = 1)
  CI_upper_perc <- round(CI_upper*100, digits = 1)
  prob_perc <- round(prob*100, digits = 1)
  print_out <- paste0(prob_perc, "%", " (", CI_lower_perc, "%",", ", CI_upper_perc, "%", ")")
  print_out
}

numbers <- perc_cis(x = 9000, n = 23000)
numbers
