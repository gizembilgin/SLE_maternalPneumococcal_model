
### incidence of health outcomes
incid_1000PY_log %>%
  group_by(scenario,age_group) %>%
  summarise(incidence = mean(incidence)) %>%
  group_by(scenario) %>%
  summarise(incidence = sum(incidence)*100)

burden_dataset_applied_U1_log %>%
  pivot_wider(names_from = scenario,
              values_from = incidence) %>%
  select(outcome,severity,no_maternal_vaccine,maternal_vaccine,incremental_effect) %>%
  group_by(outcome,severity) %>%
  summarise(no_maternal_vaccine = mean(no_maternal_vaccine),
            maternal_vaccine = mean(maternal_vaccine),
            incremental_effect = mean(incremental_effect))


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



presentations_log
