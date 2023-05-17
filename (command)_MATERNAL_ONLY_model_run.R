### This script completes one run with a maternal vaccine

incid_1000PY_vaccine_log = data.frame()
# options: "Y", Y_higher_eff_pertut, Y_lower_eff_flu, Y_lower_cov_introduction, Y_lower_cov_ANC, Y_lower_cov_tetanus_sufficient
source(paste(getwd(),"/(1)_load_model_param.R",sep=""))
source(paste(getwd(),"/(2)_configure_inital_state.R",sep=""))
source(paste(getwd(),"/(3)_pneum_ode_function.R",sep=""))
if (ageing_toggle == "cohort"){source(paste(getwd(),"/(4)_time_step.R",sep=""))
}else if (ageing_toggle == "daily"){source(paste(getwd(),"/(4)_time_step_incremental.R",sep=""))}
incid_1000PY_vaccine_log <-rbind(incid_1000PY_vaccine_log,tail(incidence_1000PY,1))

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

collapsed_result_main <- final_results %>% 
  mutate('3-5 months' = rowSums(final_results [,c(4:6)])/3,
         '6-8 months' = rowSums(final_results [,c(7:9)])/3,
         '9-11 months' = rowSums(final_results [,c(10:12)])/3) %>%
  select('0 months','1 months','2 months','3-5 months','6-8 months','9-11 months')

collapsed_result_blunting <- final_results %>% 
  mutate('0-6 months' = rowSums(final_results [,c(1:6)])/6,
         '6-12 months' = rowSums(final_results [,c(7:12)])/6,
         '1-2 years' = rowSums(final_results [,c(13:24)])/12) %>%
  select('0-6 months','6-12 months','1-2 years','5+ years')

### CONVERT TO HEALTH OUTCOMES
source(paste(getwd(),"/(5)_health_outcomes_distribution.R",sep=""))

health_outcome_0 <- data.frame(t(final_results[3,1:12]))
health_outcome_0 <- cbind(age_months = c(1:12), health_outcome_0)
colnames(health_outcome_0) <- c('age_months','vaccine_effect')
rownames(health_outcome_0) <- c()

health_outcome_0 <- merge(health_outcome_0,cdf_outcome_final,by="age_months")
health_outcome_0 <- health_outcome_0 %>%
  mutate(effect = (vaccine_effect/100)*percent_interval)

health_outcome_1 <- aggregate(health_outcome_0$effect, by=list(category=health_outcome_0$outcome), FUN=sum)
source(paste(getwd(),"/(6)_health_outcomes_num.R",sep=""))

#####################################################################

# final_results [,1:6]
# health_outcome_1
# burden_dataset_applied
# burden_dataset_applied_U1