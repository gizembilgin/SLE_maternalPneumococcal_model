### This program is intended runs the model in multiple ways to achieve the data required for the SUPPLEMENTARY MATERIAL

time.start=proc.time()[[3]] #let's see how long this runs for

### SUPPLEMENTARY MATERIAL ################################################################
#S6.1 Infant PCV cov and effectiveness #__________________________________________________
InfantPCV_tracker = data.frame()

#(A/B) PCV coverage 
pcv_coverage = "sensitivity_DTP"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
rows = collapsed_result_blunting %>% mutate(variation = 'cov',var_strength = true_cov[1])
InfantPCV_tracker = rbind(InfantPCV_tracker,rows)

pcv_coverage = "sensitivity_MCV"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
rows = collapsed_result_blunting %>% mutate(variation = 'cov',var_strength = true_cov[1])
InfantPCV_tracker = rbind(InfantPCV_tracker,rows)

pcv_coverage = "absent"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
rows = collapsed_result_blunting %>% mutate(variation = 'cov',var_strength = 0)
InfantPCV_tracker = rbind(InfantPCV_tracker,rows)

#reset 
pcv_coverage = "2019_DHS"  
N

#(B/B) PCV effectiveness 
pcv_effectiveness = "2021_upper_serotype_cov"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
rows = collapsed_result_blunting %>% mutate(variation = 'eff',var_strength = e[1])
InfantPCV_tracker = rbind(InfantPCV_tracker,rows)

pcv_effectiveness = "2021_lower_serotype_cov"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
rows = collapsed_result_blunting %>% mutate(variation = 'eff',var_strength = e[1])
InfantPCV_tracker = rbind(InfantPCV_tracker,rows)

#reset
results_warehouse[[4]] = InfantPCV_tracker
pcv_effectiveness = "2021_model"
#______________________________________________________________________________________________

#S6.2 COVID_19 sensitivity analysis____________________________________________________________
covid_sensitivity = "on"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
covid_results = collapsed_result_main
results_warehouse[[5]]  = covid_results
covid_sensitivity = "off" #(reset)
#______________________________________________________________________________________________

#S6.3 population structure  sensitivity analysis_______________________________________________
#S6.3.1 age structure
pop_distribution = "census"
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
sensitivity_pop_dn = collapsed_result_main
results_warehouse[[6]] = sensitivity_pop_dn
pop_distribution = "uniform"    

#S6.3.2 age-specific mortality
death_distribution = "2019_DHS" 
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
sensitivity_mortality= collapsed_result_main
results_warehouse[[7]] = sensitivity_mortality
death_distribution = "oldest_only"  

#S6.3.1 population growth
pop_growth_rate = 1.02439
source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
sensitivity_pop_growth = collapsed_result_main
results_warehouse[[8]] = sensitivity_pop_growth
pop_growth_rate =  1            
#______________________________________________________________________________________________

#S5.2 Length of Maternally derived Immunity
mat_wan_tracker = data.frame()
wan_list = c('slower','standard','faster')
for (docket in 1:length(wan_list)){
  mat_waning = wan_list[docket]
  source(paste(getwd(),"/(function)_full_model_run.R",sep=""))
  rows = collapsed_result_blunting %>% mutate(wan_speed = mat_waning)
  mat_wan_tracker = rbind(mat_wan_tracker,rows)
}
mat_waning = "standard"
results_warehouse[[9]] = mat_wan_tracker
###############################################################################################


time.end=proc.time()[[3]]
time.end-time.start