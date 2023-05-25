
### This script translates the impact of a maternal vaccine on incidence -> severe outcomes associated with pneumococcal 
### Dependencies: burden_disease (toggle), health_outcome_1

if (burden_disease == "base"){burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL.csv",header=TRUE)
} else if (burden_disease == "LB"){burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL_LB.csv",header=TRUE)
} else if (burden_disease == "UB"){burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL_UB.csv",header=TRUE)
} else if (burden_disease == "stochastic"){
  load(file = "1_inputs/severe_outcome_distributions.Rdata")
  burden_dataset <- severe_outcome_distributions
  
  sampled_gamma = mapply(rgamma,1,burden_dataset$gamma_shape, 1/data.frame(burden_dataset$gamma_scale))
  burden_dataset = cbind(burden_dataset, num = sampled_gamma)
  
  burden_dataset$percentage = burden_dataset$num/burden_dataset$num[burden_dataset$category == "total_pneumococcal_burden" & burden_dataset$measure == "incidence"]
  burden_dataset = burden_dataset %>% select(category,measure,num,percentage)
}

maternal_vaccine_reduction = burden_dataset %>%
  rename(severity = measure,
         outcome = category) %>%
  mutate(outcome = case_when(
    outcome == "total_pneumococcal_burden" ~ "IPD",
    outcome == "other_pneumoccocal" ~ "NPNM",
    TRUE ~ outcome
  )) %>%
  select(-percentage)
  

### Start with what we know
health_outcome_2 <- health_outcome_1 %>%
  mutate(severity = case_when(
    outcome == "death" ~ "mortality",
    outcome %in% c("hospitalised_clinical_pneumonia","radiographic_pneumonia") ~ "severe_incidence",
    TRUE ~ "incidence"
  )) %>%
  mutate(outcome = case_when(
    outcome == "death" ~ "IPD",
    outcome == "pneumococcal_meningitis" ~ "pneumoccocal_meningitis", #match spelling
    outcome %in% c("hospitalised_clinical_pneumonia","radiographic_pneumonia") ~ "pneumoccocal_pneumonia",
    TRUE ~ outcome
  )) %>%
  group_by(outcome,scenario,severity) %>%
  summarise(incidence = mean(incidence) ,.groups = "keep") %>%
  left_join(maternal_vaccine_reduction,by=c("outcome","severity")) %>%
  mutate(incidence = incidence *num)

#recall, all pneum men cases defined as severe
rows = health_outcome_2 %>%
  filter(outcome == "pneumoccocal_meningitis") %>%
  mutate(severity = "severe_incidence")
health_outcome_2 = rbind(health_outcome_2,rows)  
  

### Infer remaining categories(but don't attempt to place deaths)
#(A) Have incid for IPD overall and pneum men, but not pneumonia and NPNM
#Incid IPD = Incid pneum men + Incid pneumonia + Incid NPNM
remaining_incid = health_outcome_2 %>%
  select(-num) %>%
  filter(severity == "incidence") %>%
  pivot_wider(names_from = "outcome",
              values_from = "incidence") %>%
  mutate(remaining_incid = IPD - pneumoccocal_meningitis)

rows =  remaining_incid %>%
  mutate(outcome = "pneumoccocal_pneumonia",
         incidence = remaining_incid * (1-(burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'incidence']/
                                              burden_dataset$num[burden_dataset$category == 'pneumoccocal_pneumonia' & burden_dataset$measure == 'incidence']))) %>%
  select(scenario,severity,outcome,incidence)
health_outcome_2 = bind_rows(health_outcome_2,rows)  
rows =  remaining_incid %>%
  mutate(outcome = "NPNM",
         incidence = remaining_incid * ((burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'incidence']/
                                             burden_dataset$num[burden_dataset$category == 'pneumoccocal_pneumonia' & burden_dataset$measure == 'incidence']))) %>%
  select(scenario,severity,outcome,incidence)
health_outcome_2 = bind_rows(health_outcome_2,rows) 

#(B) Ratio NPNM severe
rows = health_outcome_2 %>%
  filter(outcome == "NPNM" & severity == "incidence") %>%
  mutate(severity = "severe_incidence") %>%
  mutate(incidence = incidence * (burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'severe_incidence']/
                                    burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'incidence']) )
health_outcome_2 = bind_rows(health_outcome_2,rows) 

#(C) Sum together severe incidence
rows = health_outcome_2 %>%
  filter(severity == "severe_incidence") %>%
  mutate(outcome = "IPD") %>%
  group_by(outcome,scenario,severity) %>%
  summarise(incidence = sum(incidence),.groups="keep")
health_outcome_2 = bind_rows(health_outcome_2,rows) 


#note is reduction in burden of disease under five, but ALL cases averted in infants
#assume uniform pop across U5
burden_dataset_applied_U1 = health_outcome_2 %>% select(-num)
burden_dataset_applied_U1$incidence = burden_dataset_applied_U1$incidence*5

# reduction in cases under one
reduction_incidence_under_one = final_results %>%
  filter(age_group %in% c("0 months", "1 months", "2 months","3 months",  "4 months",  "5 months",
                          "6 months",  "7 months",  "8 months","9 months",  "10 months",  "11 months")) %>%
  pivot_longer(cols = c("incremental_effect","maternal_vaccine","no_maternal_vaccine"),
               names_to = "scenario",
               values_to = "incidence") %>%
  group_by(scenario) %>%
  summarise(incidence = 100*mean(incidence))#now in 100,000


