### This script completes on full 'run' of the cost-effectiveness analysis
#DEPENDENCIES: burden_dataset_applied_U1, reduction_incidence_under_one,mcov
#NOTE: costs are all expressed in 2020 USD

require(ggplot2);require(tidyverse); require(beepr)

load(file = "1_inputs/healthcare_cost_estimates.Rdata")

###user toggles
costing = "rand" #options: fixed, rand
perspective = "societal" #options: healthcare,societal
complete_CEA_runs = 1000

if (costing == "fixed"){complete_CEA_runs = 1}
if (perspective == "healthcare"){healthcare_cost_estimates  = healthcare_cost_estimates %>% filter(cost_type == "direct medical")}
if (exists("MASTER_CONTROLS") == FALSE){MASTER_CONTROLS = list()}
if ("perspective" %in% names(MASTER_CONTROLS)){perspective = MASTER_CONTROLS$perspective} 

CEA_log = incremental_log = cost_averted_log = presentations_log = data.frame()

for (run_number in 1:complete_CEA_runs){
  
  
  #       (1/3) Costs of vaccine program                 
  ################################################################################
  ###(A/C) Vaccine costs
  # Note, these are all fixed costs
  price_per_dose = 8.63  # 8.63 compared to 2.90 GAVI price for PCV (sensitivity analysis)
  #price_per_dose = 2.90
  #price_per_dose = 4.5
  #price_per_dose = 0.137
  #price_per_dose = 2.90*8.63/14.5
  #price_per_dose = 8.63*0.92
  if ("vaccine_price" %in% names(MASTER_CONTROLS)){price_per_dose = MASTER_CONTROLS$vaccine_price}
  
  wastage = 0.05               # % of doses (default 0.05)
  if ("vaccine_wastage" %in% names(MASTER_CONTROLS)){wastage = MASTER_CONTROLS$vaccine_wastage}
  
  freight_costs = 0.045         # % of vaccine value
  injection_equipment = 0.044   # equipment required per dose of vaccine
  injection_equipment_wastage = 0.1 # % of injection equipment
  
  vaccine_cost_per_dose = price_per_dose * (1 + wastage) * (1 + freight_costs) + injection_equipment * (1+injection_equipment_wastage)
  vaccine_cost_100000 = mcov * 100000 * vaccine_cost_per_dose
  
  #outputs for Table 4
  # dose_cost = mcov * 100000 * price_per_dose
  # wastage_cost = mcov * 100000 * price_per_dose * (wastage + wastage*freight_costs/2)
  # freight_cost = mcov * 100000 * price_per_dose * (freight_costs + wastage*freight_costs/2)
  # injection_equipment = mcov * 100000 * injection_equipment * (1+injection_equipment_wastage)
  # dose_cost; wastage_cost;freight_cost; injection_equipment
  
  ###(B/C) Operational costs
  OC_mean = 0.95; OC_sd = 0.38
  OC_a = (OC_mean/OC_sd)^2 ; OC_b = (OC_sd^2)/OC_mean #6.25 and 0.152
  #plot(density(rgamma(10000000, OC_a, scale=OC_b)))
  
  if (costing == "fixed"){operational_costs_100000 = 100000*OC_mean
  #} else if (costing == "rand"){operational_costs_100000 = sum(rgamma(100000, OC_a, scale=OC_b))}
  } else if (costing == "rand"){operational_costs_100000 = 100000*rgamma(1, OC_a, scale=OC_b)}
  
  if ("operational_cost_multiplier" %in% names(MASTER_CONTROLS)){
    operational_costs_100000=operational_costs_100000 * MASTER_CONTROLS$operational_cost_multiplier
  }
  
  ###(C/C) Total Intervention costs
  total_intervention_costs_100000 = vaccine_cost_100000 + operational_costs_100000
  ################################################################################
  
  
  
  #       (2/3) Health system costs averted               
  ################################################################################
  if (costing == "fixed"){
    burden_disease = "base"
  } else if (costing == "rand"){
    burden_disease = "stochastic"
  }
  source(paste(getwd(),"/(6)_health_outcomes_num.R",sep=""))
  
  ###(A/D) Incidence of health outcomes/syndromes averted
  # four types of incidence averted as per Figure 1: all other ARI attributable to pneumococcal,pneumococcal pneumonia, pneumococcal meningitis, NPNM
  incidence_averted = burden_dataset_applied_U1 %>%
    filter(severity == "incidence")
  
  rows = incidence_averted %>%
    filter(outcome == "IPD") %>%
    rename(IPD = incidence) %>%
    left_join(reduction_incidence_under_one, by = "scenario") %>%
    mutate(incidence = incidence - IPD) %>%
    select(-IPD) %>%
    mutate(outcome = "total_pneumococcal_burden")
  
  incidence_averted = rbind(incidence_averted[incidence_averted$outcome != "IPD",],rows)
  #sum(incidence_averted$incidence); sum(reduction_incidence_under_one$incidence) #CHECK
  
  
  ###(B/D) Branch pneumoccocal presentations into types of care 
  if (costing == "fixed"){
    access_outpatient  = 0.857
    access_homeCare =  1- access_outpatient
    access_inpatient = 0.652
    access_inpatient_mening = access_inpatient/access_outpatient
  } else if (costing == "rand"){
    access_outpatient  = rbeta(1, 84,14)
    access_homeCare =  1- access_outpatient
    access_inpatient = rbeta(1, 60,30)
    access_inpatient_mening = access_inpatient/access_outpatient
  }
  access = data.frame(patient_type = c("outpatient", "home care", "inpatient pneumococcal pneumonia","inpatient pneumococcal meningitis"),
                      prop = c(access_outpatient,access_homeCare,access_inpatient,access_inpatient_mening))
  
  # structure as inpatient, outpatient and home care
  averted_inpatient = incidence_averted %>%
    mutate(patient_type = case_when(
      outcome %in% c( "pneumoccocal_pneumonia","NPNM" ) ~ "inpatient pneumococcal pneumonia",
      outcome == "pneumoccocal_meningitis" ~ "inpatient pneumococcal meningitis"
    )) %>%
    inner_join(access,by = "patient_type") %>%
    mutate(incidence = incidence * prop) %>%
    group_by(scenario,patient_type) %>%
    summarise(incidence = sum(incidence), .groups = "keep")
  
  averted_outpatient = incidence_averted %>%
    mutate(patient_type = "outpatient") %>%
    inner_join(access,by = "patient_type") %>%
    mutate(prop = case_when(outcome == "pneumoccocal_meningitis" ~ 1, TRUE ~ prop)) %>%
    mutate(incidence = incidence * prop) %>%
    group_by(scenario,patient_type) %>%
    summarise(incidence = sum(incidence), .groups = "keep")
  
  averted_homeCare = incidence_averted %>%
    mutate(patient_type = "home care") %>%
    inner_join(access,by = "patient_type") %>%
    mutate(prop = case_when(outcome == "pneumoccocal_meningitis" ~ 0, TRUE ~ prop)) %>%
    mutate(incidence = incidence * prop) %>%
    group_by(scenario,patient_type) %>%
    summarise(incidence = sum(incidence), .groups = "keep")
  
  presentations = rbind(averted_inpatient,averted_outpatient,averted_homeCare)
  presentations_log = rbind(presentations,presentations_log)
  #sum(incidence_averted$incidence) ;sum(averted_homeCare$incidence+averted_outpatient$incidence);# CHECK
  
  
  
  ###(C/D) 
  #initalise costs
  if (costing == "fixed"){
    
    cost_averted =  presentations %>%
      left_join(healthcare_cost_estimates, by = "patient_type", relationship = "many-to-many") %>%
      mutate(estimate = estimate * incidence) %>%
      group_by(scenario,patient_type,cost_type) %>%
      summarise(estimate = sum(estimate), .groups = "keep")

  } else if (costing == "rand"){
    
    workshop =  presentations %>%
      left_join(healthcare_cost_estimates, by = "patient_type", relationship = "many-to-many") %>%
      mutate(sampled = 0)
    
    for(row in 1:nrow(workshop)){
      workshop$sampled[row] = sum(runif(workshop$incidence[row],min=workshop$lower.quantile[row] ,max = workshop$upper.quantile[row]))
    }
    
    cost_averted = workshop %>%
      group_by(scenario,patient_type,cost_type) %>%
      summarise(estimate = sum(sampled), .groups = "keep")

  }

  ###(D/D) 
  total_cost_care_averted=sum(cost_averted$estimate[cost_averted$scenario == "incremental_effect"])
  
  if ("discounting_rate" %in% names(MASTER_CONTROLS)){this_disqaly = MASTER_CONTROLS$discounting_rate
  } else{this_disqaly = 0.03}
  if ("healthcare_cost_multiplier" %in% names(MASTER_CONTROLS)){
    total_cost_care_averted=total_cost_care_averted * MASTER_CONTROLS$healthcare_cost_multiplier
  }
  if (perspective != "healthcare"){
    life_expectancy = 55.92 #49.5 from 2015 census, 55.92 from UN estimates for 2020-2024
    if (this_disqaly >0){ave_YLL_discounted = (1/this_disqaly)*(1-exp(-this_disqaly*life_expectancy))}
    
    GNI = 490
    
    lost_productivity_due_to_premature_mortality = GNI  * ave_YLL_discounted
    
    lost_productivity_due_to_premature_mortality = burden_dataset_applied_U1 %>%
      filter(severity == "morality") %>%
      mutate(estimate = incidence * lost_productivity_due_to_premature_mortality,
             patient_type = "death",
             cost_type = "premature mortality") %>%
      ungroup() %>%
      select(-outcome,-severity,-incidence)
    
    total_cost_care_averted = total_cost_care_averted + lost_productivity_due_to_premature_mortality$estimate[lost_productivity_due_to_premature_mortality$scenario == "incremental_effect"]
    
    cost_averted = rbind(cost_averted,lost_productivity_due_to_premature_mortality)
  }
  ################################################################################
  
  
  
  #       (3/3) Cost-effectiveness Analysis               
  ################################################################################
  source(paste(getwd(),"/(function)_conversion to DALYs.R",sep=""))
  this_disqaly = 0.03
  if ("discounting_rate" %in% names(MASTER_CONTROLS)){this_disqaly = MASTER_CONTROLS$discounting_rate}
  
  total_DALYs <- conversion_to_DALYs(burden_dataset_applied_U1,
                                     reduction_incidence_under_one,
                                     discqaly = this_disqaly)
  
  #Note: this will be the cost per 100,000
  cost_final = total_intervention_costs_100000 - total_cost_care_averted
  cost_final_per_child = cost_final /(mcov * 100000)
  cost_case_averted = cost_final/reduction_incidence_under_one$incidence[reduction_incidence_under_one$scenario == "incremental_effect"]
  cost_death_averted = cost_final/burden_dataset_applied_U1$incidence[burden_dataset_applied_U1$scenario == "incremental_effect" & burden_dataset_applied_U1$severity == "morality"]
  cost_per_DALY_averted = cost_final / total_DALYs$estimate[total_DALYs$scenario == "incremental_effect"]
  cost_hospo_averted = cost_final/sum(averted_inpatient$incidence[averted_inpatient$scenario == "incremental_effect"])
  
  cost_case_averted;cost_death_averted;cost_per_DALY_averted;cost_hospo_averted
  
  cost_this_run = c(cost_final,cost_final_per_child,cost_case_averted,cost_per_DALY_averted,cost_death_averted,cost_hospo_averted)
  cost_this_run = round(cost_this_run,digits=2)
  
  
  CEA_log <-rbind(CEA_log,cost_this_run)
  incremental_log <- rbind(incremental_log,data.frame(incremental_cost = cost_final, 
                                                      incremental_DALYs = total_DALYs$estimate[total_DALYs$scenario == "incremental_effect"],
                                                      incremental_life_saved = burden_dataset_applied_U1$incidence[burden_dataset_applied_U1$scenario == "incremental_effect" & burden_dataset_applied_U1$severity == "morality"],
                                                      incremental_hosp_averted = sum(averted_inpatient$incidence[averted_inpatient$scenario == "incremental_effect"]),
                                                      incremental_case_averted = reduction_incidence_under_one$incidence[reduction_incidence_under_one$scenario == "incremental_effect"]))
  cost_log <- rbind(cost_log,c(vaccine_cost_100000,operational_costs_100000,total_cost_care_averted))

  cost_averted_log = rbind(cost_averted_log,cost_averted)
}

rownames(CEA_log) <- NULL
colnames(CEA_log) <- c('program_cost','cost per child','cost_case_averted','cost_DALY_averted','cost_death_averted','cost_hospo_averted')
CEA_log <- as.data.frame(CEA_log)

CEA_log_long <- CEA_log %>%
  pivot_longer(
    cols = program_cost:cost_hospo_averted,
    names_to = c('outcome'),
    values_to = "cost"
  ) 

rownames(cost_log) <- NULL
colnames(cost_log) <- c('vaccine_costs','operational_costs','health_system_costs')
cost_log <- as.data.frame(cost_log)

cost_log_long <- cost_log %>%
  pivot_longer(
    cols = vaccine_costs:health_system_costs,
    names_to = c('component'),
    values_to = "cost"
  ) 

if (nrow(CEA_log)>1){
  shapiro.test(CEA_log$program_cost) #p=0.271>0.05 therefore normally distributed
  #shapiro.test(cost_log$vaccine_costs)
  shapiro.test(cost_log$operational_costs)
  shapiro.test(cost_log$health_system_costs)
  
  summary_over_runs <- 
    CEA_log_long %>%
    group_by(outcome) %>%
    dplyr::summarise(average_cost = round(mean(cost),digits=2), 
                     #UCI = CI(cost)[1], 
                     #LCI = CI(cost)[3],
                     #UCI = round(mean(cost)+qnorm(.975)*sd(cost)/sqrt(complete_CEA_runs),digits=2),
                     #LCI = round(mean(cost)-qnorm(.975)*sd(cost)/sqrt(complete_CEA_runs),digits=2),
                     sd = round(sd(cost),digits=2),
                     LPI = round(mean(cost)-qt(.975,df=(complete_CEA_runs-1))*sd(cost)*sqrt(1+(1/complete_CEA_runs)),digits=2),
                     UPI = round(mean(cost)+qt(.975,df=(complete_CEA_runs-1))*sd(cost)*sqrt(1+(1/complete_CEA_runs)),digits=2)
    )
  
  incremental_summary <- incremental_log %>%
    pivot_longer(cols = colnames(incremental_log),
                 names_to = "variable",
                 values_to = "value") %>%
    group_by(variable) %>%
    summarise(sd = sd(value),
              LPI = round(mean(value)-qt(.975,df=(complete_CEA_runs-1))*sd(value)*sqrt(1+(1/complete_CEA_runs)),digits=2),
              UPI = round(mean(value)+qt(.975,df=(complete_CEA_runs-1))*sd(value)*sqrt(1+(1/complete_CEA_runs)),digits=2),
              value = mean(value)
    )
  
  incremental_ICER = data.frame()
  for (this_variable in unique(incremental_summary$variable[incremental_summary$variable != "incremental_cost"])){
    outcome = incremental_summary %>% filter(variable == this_variable)
    cost = incremental_summary %>% filter(variable == "incremental_cost")
    
    row = data.frame(
      ICER = cost$value/outcome$value,
      sd = (cost$value/outcome$value) * sqrt((cost$sd/cost$value)^2+(outcome$sd/outcome$value)^2)
    ) %>%
      mutate(LPI = round(ICER-qt(.975,df=(complete_CEA_runs-1))*sd*sqrt(1+(1/complete_CEA_runs)),digits=2),
             UPI = round(ICER+qt(.975,df=(complete_CEA_runs-1))*sd*sqrt(1+(1/complete_CEA_runs)),digits=2),
             ICER_variable = this_variable)
    
    incremental_ICER = rbind(incremental_ICER,row)
  }
  
  

  
  cost_summary <- 
    cost_log_long %>%
    group_by(component) %>%
    dplyr::summarise(average_cost = round(mean(cost),digits=2), 
                     sd = round(sd(cost),digits=2),
                     LPI = round(mean(cost)-qt(.975,df=(complete_CEA_runs-1))*sd(cost)*sqrt(1+(1/complete_CEA_runs)),digits=2),
                     UPI = round(mean(cost)+qt(.975,df=(complete_CEA_runs-1))*sd(cost)*sqrt(1+(1/complete_CEA_runs)),digits=2)
    ) 
}


#summary_over_runs
summary_over_runs[summary_over_runs$outcome == 'cost_DALY_averted',]

cost_averted_log %>%
  group_by(cost_type) %>%
  summarise(mean = mean(estimate)) %>%
  ungroup() %>%
  mutate(proportion = mean/sum(mean))

incremental_summary
incremental_ICER
# LIMITATION WARNING: this 'uncertainty' is taken from running the model x number of times with different population values for parameters
#         True stochasticity would have each individual sample from the probability distribution (lots of computational power)

##do we even need discounting if only over a year?
