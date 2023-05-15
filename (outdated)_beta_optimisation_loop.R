###################################################################
### This is the beta optimisation loop                          ###                                      
### The program fits beta to the baseline prevalence            ###
###                                                             ###
### Author: Gizem Mayis Bilgin                                  ###
### Last update: 20/08/2021                                     ###
###################################################################


ari = 0.198

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
for (i in 1:3){
  track_conditions = rep(0,6); record_fit = c(0,0)
  step = upper_bound/(10^i)
  
  for(beta in seq(from=(new_beta-step*5),to=(new_beta+step*5),by=step)){
    #run model
    source(paste(getwd(),"/(4)_time_step.R",sep=""))
    prevalence_summary = c(sum(prevalence[c(1:6)])/6,
                           sum(prevalence[c(7:12)])/6,
                           sum(prevalence[c(13:24)])/12,
                           prevalence[25],
                           prevalence[26]
                          )

    #check fit
    fit = 0
    for (j in 1:4){ #ARI provided for only <5 in DHS
      fit=fit+abs(initial_fit[j]-prevalence_summary[j]) 
      #fit=fit+abs(initial_fit[j]-prevalence_summary[j])/initial_fit[j] 
      #rel and abs both give same beta
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

}
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
#ari = 0.167
# > prevalence_summary
# [1] 0.3671358 0.4163278 0.4706761 0.2856498 0.1051883
# > beta
# this_run_fit 
# 0.00063 

#ari = 0.135
# > beta
# this_run_fit 
# 0.000509 
# > prevalence_summary
# [1] 0.29671680 0.33653289 0.38027571 0.23065103 0.08493973



