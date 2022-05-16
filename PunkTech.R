#Run fitting!
time.start=proc.time()[[3]] #let's see how long this runs for

#one run to clear all settings
source(paste(getwd(),"/(0)_command_desk.R",sep=""))

ari_list = c(0.135,0.167,0.198)

PunkNotebook = list()

for (docket in 1:length(ari_list)){
  ari = ari_list[docket]
  source(paste(getwd(),"/(9)_beta_optimisation_loop.R",sep=""))
  
  results = list(new_beta, prevalence_summary,initial_fit)
  PunkNotebook[[docket]] = results
}


time.end=proc.time()[[3]]
time.end-time.start


# > PunkNotebook
# [[1]]
# [[1]][[1]]
# this_run_fit 
# 0.000675 
# 
# [[1]][[2]]
# [1] 0.27890269 0.37575915 0.43851197 0.22950438 0.06431123
# 
# [[1]][[3]]
# [1] 0.29700 0.37800 0.33750 0.22950 0.11475
# 
# 
# [[2]]
# [[2]][[1]]
# this_run_fit 
# 0.000835 
# 
# [[2]][[2]]
# [1] 0.34490640 0.46453561 0.54231098 0.28403073 0.07959604
# 
# [[2]][[3]]
# [1] 0.36740 0.46760 0.41750 0.28390 0.14195
# 
# 
# [[3]]
# [[3]][[1]]
# this_run_fit 
# 0.00099 
# 
# [[3]][[2]]
# [1] 0.40880857 0.55043145 0.64281290 0.33689839 0.09441807
# 
# [[3]][[3]]
# [1] 0.4356 0.5544 0.4950 0.3366 0.1683
