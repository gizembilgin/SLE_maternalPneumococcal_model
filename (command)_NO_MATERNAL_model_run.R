### This script completes one run without a maternal vaccine

incid_1000PY_baseline_log = data.frame()
maternal_vaccine_on = "N"
source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
if (ageing_toggle == "cohort"){source(paste(getwd(),"/(4)_time_step.R",sep=""))
}else if (ageing_toggle == "daily"){source(paste(getwd(),"/(4)_time_step_incremental.R",sep=""))}
incid_1000PY_baseline_log <- tail(incidence_1000PY,1)