#first function
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

#second function
test_trend_ca <- function(drug, df = clean_fars){
  if(drug == "Nonalcohol") {
    
    nonalcohol <- df %>% 
      mutate(alchyesno = drug_type != "Alcohol") %>% 
      filter(alchyesno == "TRUE") %>% 
      select("unique_id", "sex", "year", "agecat", "positive_for_drug") %>% 
      mutate(drug_type = "Nonalcohol") %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = nonalcohol$positive,
                                  n = nonalcohol$trials)
    sqrt(ca_alcohol$statistic)    
    
  }
  else {
    to_test <- df %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = to_test$positive,
                                  n = to_test$trials)
    sqrt(ca_alcohol$statistic)
  }
}

#test of second function
test1 <- test_trend_ca(drug = "Stimulant")

#further test
# drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant", 
#                "Stimulant", "Cannabinoid", "Other")
# drug_trend_tests_ca <- lapply(drug_list, test_trend_ca) 
# drug_trend_tests_ca <- dplyr::bind_rows(drug_trend_tests_ca) %>%
#   dplyr::mutate(drug = drug_list) %>%
#   dplyr::select(drug, "z value", "Pr(>|z|)") 
# drug_trend_tests_ca %>% 
#   knitr::kable()

#third function

test_trend_log_reg <- function(drug, df = clean_fars){
  if(drug == "Nonalcohol") {
    
    nonalcohol <- df %>% 
      mutate(alchyesno = drug_type != "Alcohol") %>% 
      filter(alchyesno == "TRUE") %>% 
      select("unique_id", "sex", "year", "agecat", "positive_for_drug") %>% 
      mutate(drug_type = "Nonalcohol")
    log_reg <- glm(positive_for_drug ~ year, data = nonalcohol,
                   family = binomial(link = "logit"))
    x <- summary(log_reg)$coefficients
    x = as.data.frame(x)
    x %>% 
      slice(2) %>% 
      select(3:4)    
    
  }
  else {
    alcohol <- df %>%
      filter(drug_type == drug)
    log_reg <- glm(positive_for_drug ~ year, data = alcohol,
                   family = binomial(link = "logit"))
    x <- summary(log_reg)$coefficients 
    x = as.data.frame(x)
    x %>% 
      slice(2) %>% 
      select(3:4)
  }
}

