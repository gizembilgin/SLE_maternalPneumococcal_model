### This script completes on full 'run' of the cost-effectiveness analysis
#DEPENDENCIES: burden_dataset_applied_U1, reduction_incidence_under_one,mcov
#NOTE: costs are all expressed in 2020 USD

require(ggplot2);require(tidyverse)

load(file = "1_inputs/healthcare_cost_estimates.Rdata")

###user toggles
costing = "rand" #options: fixed, rand
perspective = "societal"
complete_CEA_runs = 1000

if (costing == "fixed"){complete_CEA_runs = 1}
if (perspective == "healthcare"){healthcare_cost_estimates  = healthcare_cost_estimates %>% filter(cost_type == "direct medical")}
if (exists("MASTER_CONTROLS") == FALSE){MASTER_CONTROLS = list()}
if ("perspective" %in% names(MASTER_CONTROLS)){perspective = MASTER_CONTROLS$perspective} 

cost_averted_breakdown = data.frame()

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
  incidence_averted = burden_dataset_applied_U1[burden_dataset_applied_U1$measure == 'incidence',c(1,4)]
  incidence_averted[incidence_averted$category == 'total_pneumococcal_burden',2] = round(-reduction_incidence_under_one)-incidence_averted[incidence_averted$category == 'total_pneumococcal_burden',2]
  incidence_averted = incidence_averted[,c(2)]
  incidence_averted; sum(incidence_averted); reduction_incidence_under_one #CHECK
  
  
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
  
  
  # structure as inpatient, outpatient and home care
  averted_inpatient = incidence_averted[2] * access_inpatient + incidence_averted[4] * access_inpatient
  averted_inpatient_mening = incidence_averted[3] * access_inpatient_mening
  
  averted_outpatient = incidence_averted[1] * access_outpatient +
                             incidence_averted[2] * access_outpatient +
                             incidence_averted[3] * 1 +
                             incidence_averted[4] * access_outpatient
  
  averted_homeCare = incidence_averted[1] * access_homeCare +
                             incidence_averted[2] * access_homeCare +
                             incidence_averted[3] * 0 +
                             incidence_averted[4] * access_homeCare
  
  # incidence_averted;averted_homeCare+averted_outpatient;# CHECK
  
  
  
  ###(C/D) 
  #initalise costs
  if (costing == "fixed"){
    cost_averted = rep(0,4) #inpatient inpatient_mening outpatient homecare
    cost_averted[1] = sum(healthcare_cost_estimates$estimate[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia"]) * averted_inpatient
    cost_averted[2] = sum(healthcare_cost_estimates$estimate[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis"]) * averted_inpatient_mening
    cost_averted[3] = sum(healthcare_cost_estimates$estimate[healthcare_cost_estimates$patient_type == "outpatient"]) * averted_outpatient
    cost_averted[4] = sum(healthcare_cost_estimates$estimate[healthcare_cost_estimates$patient_type == "home care"]) * averted_homeCare
  
  } else if (costing == "rand"){
    cost_averted = rep(0,4) #inpatient inpatient_mening outpatient homecare
    
    cost_averted[1] = sum(runif(averted_inpatient,
                                min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia" & healthcare_cost_estimates$cost_type == "direct medical"], 
                                max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia"& healthcare_cost_estimates$cost_type == "direct medical"]))
    cost_averted[2] = sum(runif(averted_inpatient_mening,
                                min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis"& healthcare_cost_estimates$cost_type == "direct medical"], 
                                max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis"& healthcare_cost_estimates$cost_type == "direct medical"]))
    
    cost_averted[3] = sum(runif(averted_outpatient,
                                min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "outpatient"& healthcare_cost_estimates$cost_type == "direct medical"], 
                                max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "outpatient"& healthcare_cost_estimates$cost_type == "direct medical"]))
  
    cost_averted[4] =  sum(runif(averted_homeCare,
                                 min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "home care"& healthcare_cost_estimates$cost_type == "direct medical"], 
                                 max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "home care"& healthcare_cost_estimates$cost_type == "direct medical"]))
    
    row = data.frame(cost = sum(cost_averted), category = "direct medical")
    cost_averted_breakdown = rbind(cost_averted_breakdown,row)
    
    if (perspective != "healthcare"){
      
      caregiver_income_loss = c(sum(runif(averted_inpatient,
                                        min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia" & healthcare_cost_estimates$cost_type == "caregiver income loss"], 
                                        max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia"& healthcare_cost_estimates$cost_type == "caregiver income loss"])),
                                sum(runif(averted_inpatient_mening,
                                          min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis" & healthcare_cost_estimates$cost_type == "caregiver income loss"], 
                                          max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis"& healthcare_cost_estimates$cost_type == "caregiver income loss"])))
      
      direct_non_medical = c(sum(runif(averted_inpatient,
                                       min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia" & healthcare_cost_estimates$cost_type == "direct non-medical"], 
                                       max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal pneumonia"& healthcare_cost_estimates$cost_type == "direct non-medical"])),
                             sum(runif(averted_inpatient_mening,
                                       min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis" & healthcare_cost_estimates$cost_type == "direct non-medical"], 
                                       max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "inpatient pneumococcal meningitis"& healthcare_cost_estimates$cost_type == "direct non-medical"])),
                             sum(runif(averted_outpatient,
                                       min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "outpatient" & healthcare_cost_estimates$cost_type == "direct non-medical"], 
                                       max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "outpatient" & healthcare_cost_estimates$cost_type == "direct non-medical"])),
                             sum(runif(averted_homeCare,
                                      min=healthcare_cost_estimates$lower.quantile[healthcare_cost_estimates$patient_type == "home care" & healthcare_cost_estimates$cost_type == "direct non-medical"], 
                                      max=healthcare_cost_estimates$upper.quantile[healthcare_cost_estimates$patient_type == "home care" & healthcare_cost_estimates$cost_type == "direct non-medical"]))
                             )
      
      cost_averted[1] = cost_averted[1] + caregiver_income_loss[1] + direct_non_medical[1]
      cost_averted[2] = cost_averted[2] + caregiver_income_loss[2] + direct_non_medical[2]
      cost_averted[3] = cost_averted[3] + direct_non_medical[3]
      cost_averted[4] = cost_averted[4] + direct_non_medical[4]
      
      row = data.frame(cost = sum(direct_non_medical), category = "direct non-medical")
      cost_averted_breakdown = rbind(cost_averted_breakdown,row)
      
      row = data.frame(cost = sum(caregiver_income_loss), category = "caregiver income loss")
      cost_averted_breakdown = rbind(cost_averted_breakdown,row)
    }
  }

  ###(D/D) 
  total_cost_care_averted=sum(cost_averted)
  
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
    
    total_cost_care_averted = total_cost_care_averted + lost_productivity_due_to_premature_mortality* burden_dataset_applied_U1[3,3]
    
    row = data.frame(cost = sum(total_cost_care_averted), category = "premature mortality")
    cost_averted_breakdown = rbind(cost_averted_breakdown,row)
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
  cost_case_averted = cost_final/reduction_incidence_under_one
  cost_death_averted = cost_final/-burden_dataset_applied_U1[3,3]
  cost_per_DALY_averted = cost_final / total_DALYs
  cost_hospo_averted = cost_final/-(averted_inpatient+averted_inpatient_mening)
  
  cost_case_averted;cost_death_averted;cost_per_DALY_averted;cost_hospo_averted
  
  cost_this_run = c(cost_final,cost_final_per_child,cost_case_averted,cost_per_DALY_averted,cost_death_averted,cost_hospo_averted)
  cost_this_run = round(cost_this_run,digits=2)
  
  
  if (run_number == 1){
    CEA_log <- data.frame(t(cost_this_run))
    incremental_log <- data.frame(incremental_cost = cost_final, 
                                  incremental_DALYs = total_DALYs,
                                  incremental_life_saved = burden_dataset_applied_U1[3,3],
                                  incremental_hosp_averted = (averted_inpatient+averted_inpatient_mening),
                                  incremental_case_averted = -reduction_incidence_under_one)
    cost_log <- data.frame(vaccine_cost_100000,operational_costs_100000,total_cost_care_averted)
  }
  if (run_number > 1){
    CEA_log <-rbind(CEA_log,cost_this_run)
    incremental_log <- rbind(incremental_log,data.frame(incremental_cost = cost_final, 
                                                        incremental_DALYs = total_DALYs,
                                                        incremental_life_saved = burden_dataset_applied_U1[3,3],
                                                        incremental_hosp_averted = (averted_inpatient+averted_inpatient_mening),
                                                        incremental_case_averted = -reduction_incidence_under_one))
    cost_log <- rbind(cost_log,c(vaccine_cost_100000,operational_costs_100000,total_cost_care_averted))
  }
  
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

cost_averted_breakdown %>%
  group_by(category) %>%
  summarise(mean = mean(cost)) %>%
  ungroup() %>%
  mutate(proportion = mean/sum(mean))

incremental_summary; incremental_ICER
# LIMITATION WARNING: this 'uncertainty' is taken from running the model x number of times with different population values for parameters
#         True stochasticity would have each individual sample from the probability distribution (lots of computational power)

##do we even need discounting if only over a year?
