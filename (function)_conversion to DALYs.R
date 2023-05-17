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
  discounted_YLL = ave_YLL_discounted*this_burden_dataset_applied_U1$mat_vax_reduction_u1[this_burden_dataset_applied_U1$category == "total_pneumococcal_burden" & 
                                                                                            this_burden_dataset_applied_U1$measure == 'morality'] 
  
  
  
  #### (2) Calculation of YLD
  #(A) short term respiratory illness
  if (costing == "fixed"){
    resp_length = 10/365
    severe_resp_wt = 0.133
    mod_resp_wt = 0.051
    respt_YLD = this_burden_dataset_applied_U1[1,3]*severe_resp_wt+(-this_reduction_incidence_under_one-this_burden_dataset_applied_U1[1,3])*mod_resp_wt
    respt_YLD = respt_YLD*resp_length
    
  } else if (costing == "rand"){
    resp_length = rgamma(1,shape = 21.232443, scale = 0.491891)/365
    severe_resp = sum(rbeta(this_burden_dataset_applied_U1[1,3],13.29,86.62))
    mod_resp = sum(rbeta((-this_reduction_incidence_under_one-this_burden_dataset_applied_U1[1,3]),5.09,94.77))
    respt_YLD = (severe_resp + mod_resp) * resp_length
  }
  
  
  
  
  
  
  #(B) long term sequlae
  # ave_YLL is years of life left
  if (costing == "fixed"){
    seq_prob = 0.247
    seq_wt = 0.542
    num_seq = seq_prob * this_burden_dataset_applied_U1[this_burden_dataset_applied_U1$category == 'pneumoccocal_meningitis' & this_burden_dataset_applied_U1$measure == 'incidence' , 3]
    seq_YLD = ave_YLL_discounted * seq_wt *num_seq
  } else if (costing == "rand"){
    seq_prob = rbeta(1,25,75)
    num_seq = seq_prob * this_burden_dataset_applied_U1[this_burden_dataset_applied_U1$category == 'pneumoccocal_meningitis' & this_burden_dataset_applied_U1$measure == 'incidence' , 3]
    seq_YLD = ave_YLL_discounted * sum(rbeta(num_seq,18.40972,15.55655))
  }
  
  
  
  
  
  
  #### (4) Sum of YLL + YLD
  ave_YLL*this_burden_dataset_applied_U1[3,3]; discounted_YLL
  respt_YLD
  seq_YLD 
  
  
  total_DALYs = discounted_YLL + respt_YLD + seq_YLD
  total_DALYs
  return(total_DALYs)
}
