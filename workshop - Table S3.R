
### incidence of health outcomes
incid_1000PY_log %>%
  group_by(scenario,age_group) %>%
  summarise(incidence = mean(incidence)) %>%
  group_by(scenario) %>%
  summarise(incidence = sum(incidence)*100)

burden_dataset_applied_U1_log %>%
  group_by(outcome,scenario,severity) %>%
  summarise(incidence = mean(incidence)) %>%
  pivot_wider(names_from = scenario,
              values_from = incidence) %>%
  select(outcome,severity,no_maternal_vaccine,maternal_vaccine,incremental_effect) 

presentations_log %>%
  group_by(scenario,patient_type) %>%
  summarise(incidence = mean(incidence)) %>%
  pivot_wider(names_from = scenario,
              values_from = incidence) %>%
  select(patient_type,no_maternal_vaccine,maternal_vaccine,incremental_effect) %>%
  filter(substr(patient_type,1,9) == "inpatient") %>%
  summarise(no_maternal_vaccine = sum(no_maternal_vaccine),
            maternal_vaccine  = sum(maternal_vaccine),
            incremental_effect = sum(incremental_effect))

### costs by cost type
cost_averted_log %>%
  group_by(scenario,patient_type,cost_type) %>%
  summarise(estimate = mean(estimate))%>%
  pivot_wider(names_from = scenario,
              values_from = estimate) %>%
  select(cost_type,patient_type,no_maternal_vaccine,maternal_vaccine,incremental_effect) %>%
  arrange(cost_type,patient_type)
  
intervention_cost = cost_log %>%
  mutate(intervention_cost = vaccine_costs  + operational_costs) %>%
  summarise(intervention_cost = mean(intervention_cost))
intervention_cost

#societal
cost_averted_log %>%
  group_by(scenario,patient_type,cost_type) %>%
  summarise(estimate = mean(estimate))%>%
  group_by(scenario) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(estimate = case_when(
    scenario == "no_maternal_vaccine" ~ estimate,
    scenario == "maternal_vaccine" ~ estimate + as.numeric(intervention_cost),
    scenario == "incremental_effect" ~ estimate - as.numeric(intervention_cost)
  ))

#healthcare
cost_averted_log %>%
  filter(cost_type %in% c('direct medical')) %>%
  group_by(scenario,patient_type,cost_type) %>%
  summarise(estimate = mean(estimate))%>%
  group_by(scenario) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(estimate = case_when(
    scenario == "no_maternal_vaccine" ~ estimate,
    scenario == "maternal_vaccine" ~ estimate + as.numeric(intervention_cost),
    scenario == "incremental_effect" ~ estimate - as.numeric(intervention_cost)
  ))


