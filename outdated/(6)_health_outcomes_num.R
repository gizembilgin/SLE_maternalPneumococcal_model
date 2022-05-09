burden_dataset <- read.csv("health_outcomes_num_SL.csv",header=TRUE)

# ratio of all cases to IPD cases
total_cases <- ave(as.numeric(incid_1000PY_baseline[1:6]))
total_cases <- total_cases[1]*100

total_IPD <- burden_dataset[1,3]

ratio_IPD = total_IPD/total_cases


# reduction in cases under one
reduction_incidence_under_one = ave(as.numeric(final_results[1,1:6]))
reduction_incidence_under_one = reduction_incidence_under_one[1]*100 #now in 100,000
reduction_incidence_under_one



#reduction in IPD cases
reduction_IPD_under_one <- ratio_IPD * reduction_incidence_under_one
reduction_IPD_under_one


# reduction in IPD cases under one
burden_dataset_applied <- burden_dataset %>%
  mutate(maternal_vaccine_reduction = reduction_IPD_under_one * percentage)

burden_dataset_applied<- burden_dataset_applied[,c(1,2,5)]

burden_dataset_applied$maternal_vaccine_reduction <- round(burden_dataset_applied$maternal_vaccine_reduction, digits = 0)

#COMEBACK, may have to modify by our incidence per 100,000 / study burden incidence per 100,000
burden_dataset_applied
