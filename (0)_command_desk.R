###################################################################
### This program runs all sub-scripts of the pneumococcal       ###
### model to complete one standard 'run'                        ###        
### The program was first developed in 2019 in MATLAB,          ###
### hence, I apologise for any matrix-focused clunky coding     ###
###                                                             ###
### Author: Gizem Mayis Bilgin                                  ###
### Last significant update: 09/05/2022                         ###
###################################################################


#       (1/4) Setup                 
####################################################################
#rm(list=ls()) # clear global environment
time.start=proc.time()[[3]] #let's see how long this runs for
library(deSolve)
library(ConnMatTools)
library(ggplot2)
library(dplyr)
library(tidyverse)
#####################################################################


#       (2/4) Toggles for sensitivity analysis                
####################################################################
time_step =  1           # model time-step (ODE timestep)
ageing_step = 30         # how frequently ageing is implemented
ageing_toggle = "cohort" # options:"cohort","daily"

model_years = 1  # how many years should the model run for?
complete_model_runs=1
ARI_setting="DHS_2019"
ari = 0.167  # proportion ARI attributable to pneumococcal, options: 0.135 (IPD probe), 0.167 (average), 0.198 (AOM probe), otherwise run (9) beta optimisation!
pcv_effectiveness = "2021_model"  # options: 2019_model, 2021_model, 2021_random_sample 2021_upper_serotype_cov 2021_lower_serotype_cov

PCV_start_bracket = c(3,4,5)        # options: c(3,5,7) original submission of paper 1 or c(3,4,5) revised with DHS timeliness analysis
pcv_coverage = "2019_DHS"           # options: 2017_MICS, 2019_DHS, 2019_DHS_random_sample, 2019_DHS_lower_bound, 2019_DHS_upper_bound, sensitivity_DTP, sensitivity_MCV, absent
pop_distribution = "uniform"        # choose between "uniform" or "census"
death_distribution = "oldest_only"  # choose between = "oldest_only" or "2019_DHS"
pop_growth_rate =  1                # 1 confers to no growth, estimate is 1.02439 from 2015 Population and Housing Census('Thematic report on population projections')
  
carriage_adj = 1                  # adjustment for carriage
recov_adj = 1                     # adjustment for recovery
recov_setting = "2021_model"      # options: 2019_model 2021_model 2021_lower 2021_upper
carriage_setting = "2022_model"   # options: 2019_model 2021_model 2022_model
mat_waning = "standard"           # choose between "standard", "slower" and "faster"
#blunting options 0.05 (possible), 0.15 (unlikely), 0.30 (very unlikely)
blunting_temp = 0                 # temporary reduction in PCV efficacy as a result of maternal antibody presence
blunting_long = 0                 # ongoing reduction in PCV efficacy as a result of maternal antibody presence

burden_disease ="base" #options: "base","LB","UB"

covid_sensitivity = "off"          #options: "on" or "off"
#####################################################################


#       (3/4) Run model             
####################################################################
incid_1000PY_log = data.frame()

for (run_number in 1:complete_model_runs){
  source(paste(getwd(),"/(1)_load_model_param.R",sep=""))
  # beta is the modification factor on transmission, these values have been fitted with the optimisation function
 if (ari == 0.167){
    beta = 0.00063     
  } else if (ari == 0.135){
    beta = 0.000509 
  } else if (ari == 0.198){
    beta = 0.000745 
  } else{
    stop("Your prevalence of ARI has not been fitted, run (9) optimisation")
  }
  
  maternal_vaccine_on = "N"
  source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
  source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
  if (ageing_toggle == "cohort"){source(paste(getwd(),"/(4)_time_step.R",sep=""))
  }else if (ageing_toggle == "daily"){source(paste(getwd(),"/(4)_time_step_incremental.R",sep=""))}
  prevalence_summary = c(sum(prevalence[c(1:6)])/6,
                           sum(prevalence[c(7:12)])/6,
                           sum(prevalence[c(13:24)])/12,
                           prevalence[25],
                           prevalence[26])
  baseline_run = data.frame(t(tail(incidence_1000PY,1)))
  colnames(baseline_run) <- "incidence"
  baseline_run = data.frame(incidence = baseline_run, 
                        age_group = age_group_titles,
                        scenario = "no_maternal_vaccine")
  incid_1000PY_log <-bind_rows(incid_1000PY_log,baseline_run)
 
   maternal_vaccine_on = "Y"     
   # options: "Y", Y_higher_eff_pertut, Y_lower_eff_flu, Y_lower_cov_introduction, Y_lower_cov_ANC, Y_lower_cov_tetanus_sufficient
  source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
  source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
  if (ageing_toggle == "cohort"){source(paste(getwd(),"/(4)_time_step.R",sep=""))
  }else if (ageing_toggle == "daily"){source(paste(getwd(),"/(4)_time_step_incremental.R",sep=""))} #sensitivity analysis requested by reviewer of paper 1
   matVax_run = data.frame(t(tail(incidence_1000PY,1)))
   colnames(matVax_run) <- "incidence"
   matVax_run = data.frame(incidence = matVax_run, 
                         age_group = age_group_titles,
                         scenario = "maternal_vaccine")
   incid_1000PY_log <-bind_rows(incid_1000PY_log,matVax_run)
   
   vaccine_effect = rbind(matVax_run,baseline_run) %>%
     pivot_wider(names_from = scenario,
                 values_from = incidence) %>%
     mutate(incidence = no_maternal_vaccine - maternal_vaccine,
            scenario = "incremental_effect") %>%
     select(scenario, age_group,incidence)
   incid_1000PY_log <-bind_rows(incid_1000PY_log,vaccine_effect)
  
}

final_results = incid_1000PY_log %>%
  group_by(age_group,scenario) %>%
  summarise(incidence = mean(incidence), .groups = "keep") %>%
  pivot_wider(names_from = scenario,
              values_from = incidence) %>%
  mutate(incremental_percentage = incremental_effect/no_maternal_vaccine)
#####################################################################


#      (4/4) Tabulate results      
####################################################################
collapsed_result_main <- final_results %>% 
  mutate(age_group = case_when(
    age_group %in% c("3 months",  "4 months",  "5 months") ~ "3-5 months",
    age_group %in% c("6 months",  "7 months",  "8 months") ~ "6-8 months",
    age_group %in% c("9 months",  "10 months",  "11 months") ~ "9-11 months",
    TRUE ~ age_group
  )) %>%
  filter(age_group %in% c('0 months','1 months','2 months','3-5 months','6-8 months','9-11 months')) %>%
  group_by(age_group) %>%
  summarise(incremental_effect = mean(incremental_effect),
            maternal_vaccine  = mean(maternal_vaccine ),
            no_maternal_vaccine   = mean(no_maternal_vaccine  ),
            incremental_percentage = mean(incremental_percentage))

collapsed_result_blunting <- final_results %>% 
  mutate(age_group = case_when(
    age_group %in% c("0 months", "1 months", "2 months","3 months",  "4 months",  "5 months") ~ "0-6 months",
    age_group %in% c("6 months",  "7 months",  "8 months","9 months",  "10 months",  "11 months") ~ "6-12 months",
    age_group %in% c("12 months", "13 months", "14 months","15 months",  "16 months",  "17 months",
                     "18 months", "19 months", "20 months","21 months",  "22 months",  "23 months") ~ "1-2 years",
    TRUE ~ age_group
  )) %>%
  filter(age_group %in% c("0-6 months","6-12 months", "1-2 years","5+ years")) %>%
  group_by(age_group) %>%
  summarise(incremental_effect = mean(incremental_effect),
            maternal_vaccine  = mean(maternal_vaccine ),
            no_maternal_vaccine   = mean(no_maternal_vaccine  ),
            incremental_percentage = mean(incremental_percentage))

### CONVERT TO HEALTH OUTCOMES
source(paste(getwd(),"/(5)_health_outcomes_distribution.R",sep=""))

health_outcome_0 <- final_results %>%
  filter(age_group %in% c("0 months", "1 months", "2 months","3 months",  "4 months",  "5 months",
                          "6 months",  "7 months",  "8 months","9 months",  "10 months",  "11 months")) %>%
  mutate(age_months = as.numeric(substr(age_group,1,2))+1) %>%
  #translate to % of 'normal'
  mutate(incremental_effect  = incremental_effect / no_maternal_vaccine,
         maternal_vaccine = maternal_vaccine / no_maternal_vaccine,
         no_maternal_vaccine = 1) %>%
  left_join(cdf_outcome_final, by = "age_months") %>%
  pivot_longer(cols = c("incremental_effect","maternal_vaccine","no_maternal_vaccine"),
               names_to = "scenario",
               values_to = "incidence") %>%
  mutate(incidence = incidence*percent_interval/100) %>%
  select(age_group,age_months,outcome,scenario,incidence)

health_outcome_1 <- health_outcome_0 %>%
  group_by(outcome,scenario) %>%
  summarise(incidence = sum(incidence),.groups = "keep")

source(paste(getwd(),"/(6)_health_outcomes_num.R",sep=""))
#####################################################################



collapsed_result_main

#health_outcome_1
#burden_dataset_applied_U1
#prevalence_summary

time.end=proc.time()[[3]]
time.end-time.start
#5 minutes per run (10/05/2022)

