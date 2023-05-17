
### This script translates the impact of a maternal vaccine on incidence -> severe outcomes associated with pneumococcal 

if (burden_disease == "base"){burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL.csv",header=TRUE)
} else if (burden_disease == "LB"){burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL_LB.csv",header=TRUE)
} else if (burden_disease == "UB"){burden_dataset <- read.csv("1_inputs/health_outcomes_num_SL_UB.csv",header=TRUE)
} else if (burden_disease == "stochastic"){
  load(file = "1_inputs/severe_outcome_distributions.Rdata")
  burden_dataset <- severe_outcome_distributions
  
  sampled_gamma = mapply(rgamma,1,burden_dataset$gamma_shape, 1/data.frame(burden_dataset$gamma_scale))
  burden_dataset = cbind(burden_dataset, num = sampled_gamma)
  
  burden_dataset$percentage = burden_dataset$num/burden_dataset$num[burden_dataset$category == "total_pneumococcal_burden" & burden_dataset$measure == "incidence"]
  burden_dataset = burden_dataset %>% select(category,measure,num,percentage)
}

#need to multiply % reduction in cases under five with burden under five
maternal_vaccine_reduction = rep(0,12)

###(1) Start with what we know
#we have % reductions for IPD incid, IPD deaths, hospitalized/radiographic pneumonia (assume severe), pneum meningitis
#reduction in total IPD incidence
maternal_vaccine_reduction[1] = burden_dataset$num[burden_dataset$category == 'total_pneumococcal_burden' & burden_dataset$measure == 'incidence'] *
  (health_outcome_1[health_outcome_1$category == 'IPD',2]/100)
#IPD mortality
maternal_vaccine_reduction[3] = burden_dataset$num[burden_dataset$category == 'total_pneumococcal_burden' & burden_dataset$measure == 'morality'] *
  (health_outcome_1[health_outcome_1$category == 'death',2]/100)
#severe pneumonia as average of two pneumonoia measures
maternal_vaccine_reduction[5] = burden_dataset$num[burden_dataset$category == 'pneumoccocal_pneumonia'& burden_dataset$measure == 'severe_incidence'] *
  (health_outcome_1[health_outcome_1$category == 'hospitalised_clinical_pneumonia',2]+
     health_outcome_1[health_outcome_1$category == 'radiographic_pneumonia',2])/(2*100)
#meningitis
maternal_vaccine_reduction[7] = burden_dataset$num[burden_dataset$category == 'pneumoccocal_meningitis' & burden_dataset$measure == 'severe_incidence'] *
  (health_outcome_1[health_outcome_1$category == 'pneumococcal_meningitis',2]/100)
maternal_vaccine_reduction[8] = maternal_vaccine_reduction[7] #recall, all pneum men cases defined as severe


###(2) Infer remaining (but don't attempt to place deaths)
#(A)Have incid for IPD overall and pneum men, but not pneumonia and NPNM
#Incid IPD = Incid pneum men + Incid pneumonia + Incid NPNM
remaining_incid = maternal_vaccine_reduction[1]- maternal_vaccine_reduction[7]
maternal_vaccine_reduction[4]=remaining_incid * (1-(burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'incidence']/
                                                   burden_dataset$num[burden_dataset$category == 'pneumoccocal_pneumonia' & burden_dataset$measure == 'incidence']))
maternal_vaccine_reduction[10]=remaining_incid * (burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'incidence']/
                                                       burden_dataset$num[burden_dataset$category == 'pneumoccocal_pneumonia' & burden_dataset$measure == 'incidence'])
 
#(B) Ratio NPNM severe
maternal_vaccine_reduction[11] = maternal_vaccine_reduction[10] * (burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'severe_incidence']/
                                                                     burden_dataset$num[burden_dataset$category == 'other_pneumoccocal' & burden_dataset$measure == 'incidence']) 

#(C) Sum together severe incidence
maternal_vaccine_reduction[2]=maternal_vaccine_reduction[5]+maternal_vaccine_reduction[8]+maternal_vaccine_reduction[11]
maternal_vaccine_reduction_rounded = round(maternal_vaccine_reduction,digits=0)

burden_dataset_applied <-cbind(burden_dataset,maternal_vaccine_reduction,maternal_vaccine_reduction_rounded)
burden_dataset_applied <- burden_dataset_applied[-c(6,9,12),c(1,2,5,6)]
burden_dataset_applied

#note is reduction in burden of disease under five, but ALL cases averted in infants
#assume uniform pop across U5

mat_vax_reduction_u1 = maternal_vaccine_reduction*5
mat_vax_reduction_u1_rounded = round(mat_vax_reduction_u1)
burden_dataset_applied_U1 <-cbind(burden_dataset,mat_vax_reduction_u1,mat_vax_reduction_u1_rounded)
burden_dataset_applied_U1 = burden_dataset_applied_U1 %>%
  filter(measure %in% c('incidence','severe_incidence') | (measure == "morality" & category == "total_pneumococcal_burden")) %>%
  select(-percentage,-num,)

# reduction in cases under one
reduction_incidence_under_one = mean(as.numeric(final_results[1,1:12]))
reduction_incidence_under_one = reduction_incidence_under_one*100 #now in 100,000

