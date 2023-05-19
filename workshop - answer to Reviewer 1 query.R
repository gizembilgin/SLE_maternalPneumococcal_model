
burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL.csv",header=TRUE)
cdf_outcome_final

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
    outcome == "death" ~ "morality"
  )) %>%
  left_join(burden_dataset,by = join_by(category, measure)) %>%
  select(-percent_cum,-percentage) %>%
  mutate(incidence = num * percent_interval/100,
         incidence_100000PY = incidence/P_inital[1]*100000)
workshop %>%
  select(outcome,age_months,incidence,incidence_100000PY) %>%
  rename(yearly_count = incidence)
