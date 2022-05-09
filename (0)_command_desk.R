###################################################################
### This program models a maternal vaccine against pneumococcal ###                                      
### The program was first developed in 2019 in MATLAB           ###
###                                                             ###
### Author: Gizem Mayis Bilgin                                  ###
### Last update: 05/08/2021                                     ###
###################################################################


#       (1/4) Setup                 
####################################################################
#rm(list=ls()) # clear global environment
#setwd("~/PhD/Research/1_maternal_pneumococcal/code/R")
#setwd("C:/Users/u6044061/Documents/PhD/Research/1_maternal_pneumococcal/code/R")
library(deSolve)
library(dplyr)
#####################################################################


#       (2/4) Toggles for sensitivity analysis                
####################################################################
model_years = 1  # how many years should the model run for?
complete_model_runs=1
ARI_setting="DHS_2019"
ari = 0.167  # proportion ARI attributable to pneumococcal, options: 0.135 (IPD probe), 0.167 (average), 0.198 (AOM probe), otherwise run (9) beta optimisation!
pcv_effectiveness = "2021_model"  # options: 2019_model, 2021_model, 2021_random_sample 2021_upper_serotype_cov 2021_lower_serotype_cov

#PCV_start_bracket = c(3,5,7)
PCV_start_bracket = c(3,4,5)
pcv_coverage = "2019_DHS"           # options: 2017_MICS, 2019_DHS, 2019_DHS_random_sample, 2019_DHS_lower_bound, 2019_DHS_upper_bound, sensitivity_DTP, sensitivity_MCV, absent
pop_distribution = "uniform"        # choose between "uniform" or "census"
death_distribution = "oldest_only"  #choose between = "oldest_only" or "2019_DHS"
pop_growth_rate =  1              # 1 confers to no growth, estimate is 1.02439 from 2015 Population and Housing Census('Thematic report on population projections')
  
carriage_adj = 1                  # adjustment for carriage
recov_adj = 1                     # adjustment for recovery
recov_setting = "2021_model"      # options: 2019_model 2021_model 2021_lower 2021_upper
carriage_setting = "2022_model"      # options: 2019_model 2021_model 2022_model
contact_setting = "2021_model"    # options: 2019_model 2021_model
transmission_setting = "2021_model"    # options: 2019_model 2021_model
mat_waning = "standard"           # choose between "standard", "slower" and "faster"
#blunting options 0.05 (possible), 0.15 (unlikely), 0.30 (very unlikely)
blunting_temp = 0                 # reduction in PCV efficacy as a result of maternal antibody presence
blunting_long = 0                # COMEBACK, need full model instead of quick workaround

burden_disease ="base" #options: "base","LB","UB"

covid_sensitivity = "off"          #options: "on" or "off"
#####################################################################


#       (3/4) Run model             
####################################################################
for (run_number in 1:complete_model_runs){
  source(paste(getwd(),"/(1)_load_model_param.R",sep=""))
  # beta is the modification factor on transmission, these values have been fitted with the optimisation function
  if (ari == 0.175){
    beta =  0.0005505686 
  } else if (ari == 0.143){
    beta =  0.00045 
  } else if (ari == 0.207){
    beta = 0.0006514444  
  } else if (ari == 0.167){
    beta = 0.000525444
  } else if (ari == 0.135){
    beta = 0.000424711
  } else if (ari == 0.198){
    beta = 0.000624444
  } else{
    stop("Your prevalence of ARI has not been fitted, run (9) optimisation")
  }
  
  maternal_vaccine_on = "N"
  source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
  source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
  source(paste(getwd(),"/(4)_time_step.R",sep=""))
  if (run_number == 1){
    incid_1000PY_baseline_log <- tail(incidence_1000PY,1)
  }
  if (run_number > 1){
    incid_1000PY_baseline_log <-rbind(incid_1000PY_baseline_log,tail(incidence_1000PY,1))
  }
 
   maternal_vaccine_on = "Y"     
   # options: "Y", Y_higher_eff_pertut, Y_lower_eff_flu, Y_lower_cov_introduction, Y_lower_cov_ANC, Y_lower_cov_tetanus_sufficient
  source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
  source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
  source(paste(getwd(),"/(4)_time_step.R",sep=""))
  if (run_number == 1){
    incid_1000PY_vaccine_log <- tail(incidence_1000PY,1)
  }
  if (run_number > 1){
    incid_1000PY_vaccine_log <-rbind(incid_1000PY_vaccine_log,tail(incidence_1000PY,1))
  }
  
}

vaccine_effect_log <- incid_1000PY_vaccine_log - incid_1000PY_baseline_log

incid_1000PY_vaccine <- colMeans(incid_1000PY_vaccine_log)
incid_1000PY_baseline <- colMeans(incid_1000PY_baseline_log)
#####################################################################


#      (4/4) Tabulate results      
####################################################################
vaccine_effect_abs <- colMeans(vaccine_effect_log)
vaccine_effect_abs = round(vaccine_effect_abs, digits = 2)

vaccine_effect_sd <- apply(vaccine_effect_log,2,sd)

vaccine_effect_percentage <- 100*(incid_1000PY_baseline - incid_1000PY_vaccine)/incid_1000PY_baseline
vaccine_effect_percentage = round(vaccine_effect_percentage, digits = 2)

final_results <- as.data.frame(rbind(vaccine_effect_abs,vaccine_effect_sd,vaccine_effect_percentage))
colnames(final_results) <- age_group_titles
rownames(final_results) <-c('vaccine effect per 1000PY','vaccine effect S.D. per 1000PY','vaccine effect %')
final_results <- round(final_results, digits = 2)

collapsed_result <- final_results
collapsed_result$age_band_one <-(final_results [,1]+final_results [,2]+final_results [,3]+final_results [,4]+final_results [,5]+final_results [,6])/6
collapsed_result$age_band_two <-(final_results [,7]+final_results [,8]+final_results [,9])/3
collapsed_result$age_band_three <- (final_results [,10]+final_results [,11]+final_results [,12] +final_results [,13]+final_results [,14] +final_results [,15])/6
collapsed_result <- collapsed_result[,c('age_band_one','age_band_two','age_band_three')]
colnames(collapsed_result) <- c('0-6 months','6-12 months','1-2 years')




### CONVERT TO HEALTH OUTCOMES
# source(paste(getwd(),"/(5)_health_outcomes_distribution.R",sep=""))
# 
# health_outcome_0 <- data.frame(t(final_results[3,1:6]))
# health_outcome_0 <- cbind(age_months = c(2,4,6,8,10,12), health_outcome_0)
# colnames(health_outcome_0) <- c('age_months','vaccine_effect')
# rownames(health_outcome_0) <- c()
# 
# health_outcome_0 <- merge(health_outcome_0,cdf_outcome_final,by="age_months")
# health_outcome_0 <- health_outcome_0 %>% 
#   mutate(effect = (vaccine_effect/100)*percent_interval)
# 
# health_outcome_1 <- aggregate(health_outcome_0$effect, by=list(category=health_outcome_0$outcome), FUN=sum)
# source(paste(getwd(),"/(6)_health_outcomes_num.R",sep=""))

#####################################################################


final_results [,1:6]

# health_outcome_1
# burden_dataset_applied
# burden_dataset_applied_U1


