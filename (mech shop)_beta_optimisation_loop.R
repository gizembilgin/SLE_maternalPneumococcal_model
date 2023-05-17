###################################################################
### The program finds the beta which best fits the transmission ###
###  model to the baseline pneumococcal-attributable prevalence ###
###  of acute respiratory infections in Sierra Leone            ###
###                                                             ###
### Author: Gizem Mayis Bilgin                                  ###
###################################################################


maternal_vaccine_on = "N"
source(paste(getwd(),"/(1)_load_model_param.R",sep=""))
source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
#####################################################################


#       (3/4) The loop! Here we go =)             
####################################################################
initial_fit = c(100*inital_infection[1],
                100*inital_infection[7],
                100*inital_infection[13],
                100*inital_infection[25],
                100*inital_infection[26])

lower_bound = 0 ; upper_bound =0.001  # first test interval (i.e. beta search interval)
new_beta=(upper_bound-lower_bound)/2  # first tested beta


time.start=proc.time()[[3]] #let's see how long this runs for
minimise_this <- function(beta_input){
  
  source(paste(getwd(),"/(command)_NO_MATERNAL_model_run.R",sep=""))
  
  prevalence_summary = c(sum(prevalence[c(1:6)])/6,
                           sum(prevalence[c(7:12)])/6,
                           sum(prevalence[c(13:24)])/12,
                           prevalence[25],
                           prevalence[26]
    )
  
  fit = sum((initial_fit - prevalence_summary)^2)
  
 return(fit) 
}
save_optim = optim(0.000619,fn=minimise_this,gr=NULL)
save_optim
time.end=proc.time()[[3]]
time.end-time.start


beta_optimised = new_beta
#####################################################################


#       (4/4) check how we went          
####################################################################
beta=new_beta
beta

# beta = rep(new_beta,num_age_groups)
# beta[c(13:24)] = beta[c(13:24)] *0.9 

#source(paste(getwd(),"/(4)_time_step.R",sep=""))
source(paste(getwd(),"/(command)_NO_MATERNAL_model_run.R",sep=""))
prevalence_summary = c(sum(prevalence[c(1:6)])/6,
                       sum(prevalence[c(7:12)])/6,
                       sum(prevalence[c(13:24)])/12,
                       prevalence[25],
                       prevalence[26])
prevalence_summary
initial_fit
initial_fit - prevalence_summary
sum(initial_fit - prevalence_summary)

time.end=proc.time()[[3]]
time.end-time.start
#time = 2.2 HOURS in current form (10/05/2022)


#       (4/4) check how we went   
# beta
# 0.000619 
# prevalence_summary
# [1] 0.3624715 0.4675217 0.5563913 0.2841241 0.1036431
# 
# > initial_fit
# [1] 0.36740 0.46760 0.41750 0.28390 0.14195




