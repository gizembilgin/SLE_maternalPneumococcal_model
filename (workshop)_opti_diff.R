
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
ari = 0.175  # proportion ARI attributable to pneumococcal
pcv_effectiveness = "2021_model"  # options: 2019_model, 2021_model, 2021_random_sample 2021_upper_serotype_cov 2021_lower_serotype_cov

pcv_coverage = "2019_DHS"           # options: 2017_MICS, 2019_DHS, 2019_DHS_random_sample, 2019_DHS_lower_bound, 2019_DHS_upper_bound
pop_distribution = "uniform"        # choose between "uniform" or "census"
death_distribution = "oldest_only"  #choose between = "oldest_only" or "2019_DHS"
pop_growth_rate =  1               # 1 confers to no growth, estimate is 1.02439 from 2015 Population and Housing Census('Thematic report on population projections')

carriage_adj = 1                  # adjustment for carriage
recov_adj = 1                     # adjustment for recovery
recov_setting = "2021_model"      # options: 2019_model 2021_model 2021_lower 2021_upper
carriage_setting = "2021_model"      # options: 2019_model 2021_model
contact_setting = "2021_model"    # options: 2019_model 2021_model
transmission_setting = "2021_model"    # options: 2019_model 2021_model
mat_waning = "standard"           # choose between "standard", "slower" and "faster"
blunting = 0                      # reduction in PCV efficacy as a result of maternal antibody presence

maternal_vaccine_on = "N"
source(paste(getwd(),"/(1)_load_model_param.R",sep=""))
source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
#####################################################################


#       (3/4) The loop! Here we go =)             
####################################################################
initial_fit = c(100*inital_infection[1],
                100*inital_infection[4],
                100*inital_infection[7],
                100*inital_infection[13],
                100*inital_infection[14])

lower_bound = 0 ; upper_bound =0.001  # first test interval (i.e. beta search interval)
new_beta=(upper_bound-lower_bound)/2  # first tested beta


time.start=proc.time()[[3]] #let's see how long this runs for
track_conditions = rep(0,6); record_fit = c(0,0)

for (i in 1:10){
  step = upper_bound/(10^i)
  
  for(beta in seq(from=(new_beta-step*5),to=(new_beta+step*5),by=step)){
    #run model
    source(paste(getwd(),"/(4)_time_step.R",sep=""))
    prevalence_summary = c((prevalence[1]+prevalence[2]+prevalence[3])/3,
                           (prevalence[4]+prevalence[5]+prevalence[6])/3,
                           (prevalence[7]+prevalence[8]+prevalence[9]+prevalence[10]+prevalence[11]+prevalence[12])/6,
                           prevalence[13],
                           prevalence[14]
    )
    
    #check fit
    fit = 0
    for (j in 1:4){ #ARI provided for only <5 in DHS
      fit=fit+abs(initial_fit[j]-prevalence_summary[j])  
      #fit=fit+abs(initial_fit[j]-prevalence_summary[j])/initial_fit[j]  
    }
    this_run_fit=c(fit,beta)
    prevalence=c(prevalence_summary,beta)
    
    record_fit <- rbind(record_fit,this_run_fit)
    track_conditions <- rbind(track_conditions,prevalence)
  }
  # remove initalising row of zeros
  track_conditions = track_conditions[-1,] 
  record_fit = record_fit[-1,]
  
  min_index <- which(record_fit[,1] == min(record_fit[,1]))
  if (length(min_index)>1){warning("multiple beta search paths")}
  
  new_beta=record_fit[min_index,2]
  if (length(new_beta)>1 & new_beta[1] == new_beta[2]){
    new_beta = new_beta[1]
  }
  
}
beta_optimised = new_beta
#####################################################################


#       (4/4) check how we went          
####################################################################
beta=new_beta
beta

source(paste(getwd(),"/(4)_time_step.R",sep=""))
prevalence_summary = c((prevalence[1]+prevalence[2]+prevalence[3])/3,
                       (prevalence[4]+prevalence[5]+prevalence[6])/3,
                       (prevalence[7]+prevalence[8]+prevalence[9]+prevalence[10]+prevalence[11]+prevalence[12])/6,
                       prevalence[13],
                       prevalence[14])
prevalence_summary

time.end=proc.time()[[3]]
time.end-time.start
#time = 851 seconds in current form











