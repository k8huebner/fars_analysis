#third function

test_trend_log_reg <- function(drug, df = clean_fars){
  if(drug == "Nonalcohol") {
    
    nonalcohol <- df %>% 
      mutate(alchyesno = drug_type != "Alcohol") %>% 
      filter(alchyesno == "TRUE") %>% 
      select("unique_id", "sex", "year", "agecat", "positive_for_drug") %>% 
      mutate(drug_type = "Nonalcohol")
    log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
   x <- summary(log_reg)$coefficients
   x = as.data.frame(x)
   x %>% 
     slice(2) %>% 
     select(3:4)    
    
  }
  else {
    to_test <- df %>%
      filter(drug_type == drug)
    log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
    x <- summary(log_reg)$coefficients 
    x = as.data.frame(x)
    x %>% 
      slice(2) %>% 
      select(3:4)
  }
}

#test_trend_log_reg(drug = "Stimulant")

#fars analysis
drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
               "Stimulant", "Cannabinoid", "Other")
drug_trend_tests_log_reg <- lapply(drug_list, test_trend_log_reg)
drug_trend_tests_log_reg <- dplyr::bind_rows(drug_trend_tests_log_reg) %>%
  dplyr::mutate(drug = drug_list) %>%
  dplyr::select(drug, "z value", "Pr(>|z|)")
drug_trend_tests_log_reg %>% 
  knitr::kable(digits = 2)

