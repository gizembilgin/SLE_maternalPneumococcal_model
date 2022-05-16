### This program is intended runs the model in multiple ways to achieve the data required for MAIN PAPER
##Note: current run time is 40 minutes

results_warehouse = list()
time.start.FleetAdmiral=proc.time()[[3]] #let's see how long this runs for

### MAIN PAPER ####################################################################################
## TABLE 2 (6 runs)
#original run
source(paste(getwd(),"/(0)_command_desk.R",sep=""))
primary_model = list(collapsed_result_main,
                     collapsed_result_blunting,
                     final_results,
                     burden_dataset_applied_U1,
                     health_outcome_1)
results_warehouse[[1]] = primary_model

MatVax_tracker = data.frame()
#(A/B) Maternal vaccine coverage (90%, 85%, 70%)
maternal_vaccine_on = "Y_lower_cov_ANC"
source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_main %>% mutate(variation = 'cov',var_strength = mcov)
  MatVax_tracker = rbind(MatVax_tracker,rows)

maternal_vaccine_on = "Y_lower_cov_tetanus_sufficient"
source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_main %>% mutate(variation = 'cov',var_strength = mcov)
  MatVax_tracker = rbind(MatVax_tracker,rows)

maternal_vaccine_on = "Y_lower_cov_introduction"
source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_main %>% mutate(variation = 'cov',var_strength = mcov)
  MatVax_tracker = rbind(MatVax_tracker,rows)

#(B/B) Maternal vaccine effectiveness (90%, 50%)
maternal_vaccine_on = "Y_higher_eff_pertut"
source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_main %>% mutate(variation = 'eff',var_strength = meff)
  MatVax_tracker = rbind(MatVax_tracker,rows)

maternal_vaccine_on = "Y_lower_eff_flu"
source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_main %>% mutate(variation = 'eff',var_strength = meff)
  MatVax_tracker = rbind(MatVax_tracker,rows)

results_warehouse[[2]] = MatVax_tracker

maternal_vaccine_on = "Y" #reset  
## TABLE 3 (output of original)

## TABLE 4 (output of original)

## TABLE 5 (6 runs + original) #__________________________________________________________________
blunting_tracker = data.frame()
blunting_list = c(0.05,0.15,0.3)

#(A/B) Temporary blunting (5%,15%,30%)
for (docket in 1:length(blunting_list)){
  blunting_temp = blunting_list[docket]
  source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_blunting %>% mutate(blunting_type = 'temp',blunting_strength = blunting_list[docket],death_effect = health_outcome_1$x[health_outcome_1$category == 'death'])
  blunting_tracker = rbind(blunting_tracker,rows)
}
blunting_temp = 0

#(B/B) Continued blunting (5%,15%,30%)
for (docket in 1:length(blunting_list)){
  blunting_long = blunting_list[docket]
  source(paste(getwd(),"/(function)_MATERNAL_ONLY_model_run.R",sep=""))
  rows = collapsed_result_blunting %>% mutate(blunting_type = 'long',blunting_strength = blunting_list[docket],death_effect = health_outcome_1$x[health_outcome_1$category == 'death'])
  blunting_tracker = rbind(blunting_tracker,rows)
}
blunting_long = 0 

results_warehouse[[3]] = blunting_tracker
#______________________________________________________________________________________________
###############################################################################################


time.end.FleetAdmiral=proc.time()[[3]]
time.end.FleetAdmiral-time.start.FleetAdmiral
