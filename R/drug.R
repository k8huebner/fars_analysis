library(tidyverse)
to_test <- clean_fars %>%
  filter(drug_type == "Alcohol")
log_reg <- glm(positive_for_drug ~ year, data = to_test,
               family = binomial(link = "logit"))
summary(log_reg)$coefficients

clean_fars1 <- clean_fars %>% 
  mutate(Alcohol = drug_type != "Alcohol")
if drug == "Nonalcohol"
else drug == c("Alcohol, Cannabinoid", "Depressant", "Narcotic", "Other", "Stimulant")


<- function(){
  # Print out today's date
  cat("Today's date is: ")
  cat(format(Sys.time(), "%b %d, %Y."), "\n")
  # Add something based on the weekday of today's date
  todays_wday <- lubridate::wday(Sys.time())
  if(todays_wday %in% c(1, 7)){ # What to do on Sat / Sun
    cat("It's the weekend!")
  } else if (todays_wday == c(6)) { # What to do on Friday
    cat("It's almost the weekend!")
  } else { # What to do other days
    cat("It's ", 7 - todays_wday, "days until the weekend.")
  }
}


<- function(drug, df){
  if(df$drug == "Nonalcohol") {}
  if(df$drug %in% c("Alcohol, Cannabinoid", "Depressant", 
                 "Narcotic", "Other", "Stimulant"){
    
  }
   
}


test <- clean_fars %>% 
  mutate(alchyesno = drug_type != "Alcohol") %>% 
  filter(alchyesno == "TRUE") %>% 
  select("unique_id", "sex", "year", "agecat", "positive_for_drug") %>% 
  mutate(drug_type = "Nonalcohol")
  

summary(test)
