
### Two data frames underlying the incidence of severe disease in the model
#(1/2) estimate of the yearly burden of pneumococcal disease in Sierra Leone (https://doi.org/10.1016/S2214-109X(18)30247-X)
burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL.csv",header=TRUE)
#(2/2) distribution of pneumococcal disease by age 
#Sources: SAGE review https://blogs.lshtm.ac.uk/vaccineschedules/files/6.-Russel-review-age-specific-epidemiology-PCV-schedules-session-nov11.pdf & https://doi.org/10.1016/j.vaccine.2019.07.063
load(file = "1_inputs/cdf_outcome_final.Rdata")

### Calculating incidence rates of pneumococcal-associated illness among children under 1 month, and children between 1-2 months:
workshop = cdf_outcome_final %>%
  filter(age_months <3) %>%
  mutate(category = case_when(
    outcome %in% c("IPD","death") ~ "total_pneumococcal_burden",
    outcome %in% c("hospitalised_clinical_pneumonia","radiographic_pneumonia") ~ "pneumoccocal_pneumonia",
    outcome == "pneumococcal_meningitis" ~ "pneumoccocal_meningitis"
  ),
  measure = case_when(
    outcome %in% c("hospitalised_clinical_pneumonia","radiographic_pneumonia","pneumococcal_meningitis") ~ "incidence",
    outcome == "IPD" ~ "severe_incidence",
    outcome == "death" ~ "mortality"
  )) %>%
  left_join(burden_dataset,by = join_by(category, measure)) %>%
  select(-percent_cum,-percentage) %>%
  mutate(incidence = num * percent_interval/100,
         incidence_100000PY = incidence/P_inital[1]*100000)
workshop %>%
  select(outcome,age_months,incidence,incidence_100000PY) %>%
  rename(yearly_count = incidence)
