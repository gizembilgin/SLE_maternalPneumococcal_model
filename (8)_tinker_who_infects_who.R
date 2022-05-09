###################################################################
### This is the beta optimisation loop                          ###                                      
### The program fits beta to the baseline prevalence            ###
###                                                             ###
### Author: Gizem Mayis Bilgin                                  ###
### Last update: 20/08/2021                                     ###
###################################################################


#       (1/4) Setup                 
####################################################################
#rm(list=ls()) # clear global environment
setwd("~/PhD/Research/1_maternal vaccines/pneumococcal/code/R")                              #Dell Inspiron rootpath (personal computer)
#setwd("C:/Users/u6044061/Documents/PhD/Research/1_maternal vaccines/pneumococcal/code/R")   #Dell Latitude rootpath (ANU laptop)
library(deSolve)
library(dplyr)
#####################################################################


#       (2/4) Toggles                
####################################################################
model_years = 2  # how many years should the model run for?

ARI_setting="DHS_2019"
ari = 0.198  # proportion ARI attributable to pneumococcal, options: 0.143 (IPD probe), 0.175 (average), 0.207 (AOM probe), otherwise run (9) beta optimisation!
pcv_effectiveness = "2021_model"  # options: 2019_model, 2021_model, 2021_random_sample 2021_upper_serotype_cov 2021_lower_serotype_cov

pcv_coverage = "2019_DHS"           # options: 2017_MICS, 2019_DHS, 2019_DHS_random_sample, 2019_DHS_lower_bound, 2019_DHS_upper_bound
pop_distribution = "uniform"        # choose between "uniform" or "census"
death_distribution = "oldest_only"  #choose between = "oldest_only" or "2019_DHS"
pop_growth_rate =  1               # 1 confers to no growth, estimate is 1.02439 from 2015 Population and Housing Census('Thematic report on population projections')

carriage_adj = 1                 # adjustment for carriage
recov_adj = 1                     # adjustment for recovery
recov_setting = "2021_model"      # options: 2019_model 2021_model 2021_lower 2021_upper
carriage_setting = "2021_model"      # options: 2019_model 2021_model
contact_setting = "2021_model"    # options: 2019_model 2021_model
transmission_setting = "2021_model"    # options: 2019_model 2021_model
mat_waning = "standard"           # choose between "standard", "slower" and "faster"
blunting_temp = 0                    # reduction in PCV efficacy as a result of maternal antibody presence
blunting_long = 0               # COMEBACK, need full model instead of quick workaround

maternal_vaccine_on = "N"
source(paste(getwd(),"/(1)_load_model_param.R",sep=""))
source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
#####################################################################


#beta = 0.00055 #ARI = 17.5
#beta =  0.000525444 #ARI = 16.7
beta = 0.000624444
 


source(paste(getwd(),"/(4)_time_step.R",sep=""))
prevalence_summary = c((prevalence[1]+prevalence[2]+prevalence[3])/3,
                       (prevalence[4]+prevalence[5]+prevalence[6])/3,
                       (prevalence[7]+prevalence[8]+prevalence[9]+prevalence[10]+prevalence[11]+prevalence[12])/6,
                       prevalence[13],
                       prevalence[14])
prevalence_summary

incid <- tail(incidence_1000PY,1)
#incid











