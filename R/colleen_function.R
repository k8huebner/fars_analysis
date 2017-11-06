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

print()


function(drug,df){
   if(df$drug == c("Alcohol", "Cannabinoid", "Depressant", "Narcotic", "Other", "Stimulant")){
     
   }
 if(df$drug == "Nonalcohol"){
  Nonalcohol <- c("Cannabinoid", "Depressant", "Narcotic", "Other", "Stimulant") 
 }

 test<- if(df$drug == "Nonalcohol"){
   Nonalcohol <- c("Cannabinoid", "Depressant", "Narcotic", "Other", "Stimulant") }
summary(test)
lapply 
#to make table 

