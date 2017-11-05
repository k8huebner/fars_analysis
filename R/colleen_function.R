library(stats)
library(tidyr)
#prop.trend.test

to_test <- clean_fars %>% 
  filter(drug_type == "Alcohol") %>% 
  group_by (year) %>% 
  summarize(positive = sum(positive_for_drug, na.rm = TRUE),
            trials = sum(!is.na(positive_for_drug)))
ca_alcohol <- prop.trend.test(x = to_test$positive,
                              n = to_test$trials)
#reports Z^2 and p-value 
sqrt(ca_alcohol$statistic)
#reports Z

ca_alcohol$p.value

test <- function(data = clean_fars, drug_type = "Alcohol"){
  filter(drug_type == "drug") %>% 
  group_by (year) %>% 
  summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  ca_alcohol <- prop.trend.test(x = to_test$positive,
                                n = to_test$trials)
}

# = 
perc_cis <- function(x, n) {
  
  prob <- (x / n)
  SE <- sqrt((prob*(1-prob)/n))
  CI_lower <- prob + 1.96 * (SE*prob)
  CI_upper <- prob - 1.96 * (SE*prob)
  
  CI <- c(CI_lower, CI_upper)
  CI
  
}