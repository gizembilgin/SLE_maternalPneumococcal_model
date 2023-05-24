#### This script converts model outcomes to DALYs in four steps
#### (1) Calculation of YLL
#### (2) Calculation of YLD
#### (3) Application of discounting
#### (4) Sum of YLL + YLD

conversion_to_DALYs <- function(this_burden_dataset_applied_U1,
                                this_reduction_incidence_under_one,
                                discqaly=0.03){
  life_expectancy = 55.92 #49.5 from 2015 census, 55.92 from UN estimates for 2020-2024
  
  
  #### (1) Calculation of YLL
  #(A) average YLL lost
  # distribution of deaths prevented
  # death_dn_U1 = health_outcome_0$effect[health_outcome_0$outcome == 'death' & health_outcome_0$age_months <=12]
  # death_dn_U1 = death_dn_U1/sum(death_dn_U1)
  # 
  # sum = 0
  # for (i in 1:12){sum = sum + (life_expectancy-(i)/12)*death_dn_U1[i]}
  # ave_YLL = sum
  # here, ave_YLL = 55.60 but over engineered because this is an estimate for YLL for those <1 year of age
  ave_YLL = life_expectancy
  
  #(B) apply discounting
  #continuous approach, as per larson et al.
  if (discqaly >0){ave_YLL_discounted = (1/discqaly)*(1-exp(-discqaly*ave_YLL))
  } else if (discqaly == 0){ave_YLL_discounted = ave_YLL}
  
  #(C) calculate for 100,000
  discounted_YLL = this_burden_dataset_applied_U1 %>%
    filter(severity == "morality") %>%
    mutate(estimate = incidence * ave_YLL_discounted) %>%
    ungroup() %>%
    select(scenario,estimate)

  
  #### (2) Calculation of YLD
  #(A) short term respiratory illness
  if (costing == "fixed"){
    resp_length = 10/365
    severe_resp_wt = 0.133
    mod_resp_wt = 0.051
    
    respt_YLD = this_burden_dataset_applied_U1 %>%
      filter(severity == "incidence" & outcome == "IPD") %>%
      rename(IPD = incidence) %>%
      left_join(this_reduction_incidence_under_one, by = "scenario") %>%
      mutate(estimate = IPD * severe_resp_wt + (incidence - IPD) * mod_resp_wt) %>%
      mutate(estimate = estimate * resp_length) %>%
      ungroup() %>%
      select(scenario,estimate)

  } else if (costing == "rand"){
    resp_length = rgamma(1,shape = 21.232443, scale = 0.491891)/365
    
    respt_YLD = this_burden_dataset_applied_U1 %>%
      filter(severity == "incidence" & outcome == "IPD") %>%
      rename(IPD = incidence) %>%
      left_join(this_reduction_incidence_under_one, by = "scenario") %>%
      mutate(nonIPD = incidence - IPD,
             estimate = 0)
    
    for (row in 1:nrow(respt_YLD)){
      respt_YLD$estimate[row] = sum(rbeta(respt_YLD$IPD[row],13.29,86.62)) + sum(rbeta(respt_YLD$nonIPD[row],5.09,94.77))
    }
    respt_YLD$estimate = respt_YLD$estimate  * resp_length
  }
  

  
  #(B) long term sequlae
  # ave_YLL is years of life left
  if (costing == "fixed"){
    seq_prob = 0.247
    seq_wt = 0.542
    
    seq_YLD = this_burden_dataset_applied_U1 %>%
      filter(outcome == "pneumoccocal_meningitis" & 
               severity == "incidence") %>%
      mutate(estimate = incidence * seq_prob) %>%
      mutate(estimate = estimate * ave_YLL_discounted * seq_wt)

  } else if (costing == "rand"){
    seq_prob = rbeta(1,25,75)
    
    seq_YLD = this_burden_dataset_applied_U1 %>%
      filter(outcome == "pneumoccocal_meningitis" & 
               severity == "incidence") %>%
      mutate(estimate = incidence * seq_prob)
    
    for (row in 1:nrow(seq_YLD)){
      seq_YLD$estimate[row] = sum(rbeta(seq_YLD$estimate[row],18.40972,15.55655))
    }
    seq_YLD$estimate = seq_YLD$estimate * ave_YLL_discounted 
  }
  
  
  
  
  
  
  #### (4) Sum of YLL + YLD
  discounted_YLL = discounted_YLL %>% mutate(DALY_source = "YLL")
  respt_YLD  = respt_YLD %>% mutate(DALY_source = "respt_YLD")
  seq_YLD   = seq_YLD %>% mutate(DALY_source = "seq_YLD")
  
  
  total_DALYs = bind_rows(discounted_YLL,respt_YLD,seq_YLD) %>%
    group_by(scenario) %>%
    summarise(estimate = sum(estimate))
  total_DALYs
  return(total_DALYs)
}
