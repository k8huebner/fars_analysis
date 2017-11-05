#third function

functionmaybe <- function(drug, df = clean_fars){
  if(drug == "Nonalcohol") {
    
    nonalcohol <- df %>% 
      mutate(alchyesno = drug_type != "Alcohol") %>% 
      filter(alchyesno == "TRUE") %>% 
      select("unique_id", "sex", "year", "agecat", "positive_for_drug") %>% 
      mutate(drug_type = "Nonalcohol")
    log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
    summary(log_reg)$coefficients
    
    
  }
  else {
    to_test <- df %>%
      filter(drug_type == drug)
    log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
    summary(log_reg)$coefficients
  }
}
   

functionmaybe(drug = "Other")

