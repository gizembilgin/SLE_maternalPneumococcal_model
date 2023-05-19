
### This script configures the initial state of the pneumococcal transmission model

###### (1/4) intialise maternal vaccine characteristics     
if (maternal_vaccine_on == "Y"){
  mcov = 0.9743  # maternal vaccine coverage, currently assumed to be equal to single dose of tetanus during pregnancy
  meff = 0.75    # maternal vaccine effectiveness
  #meff = 0.81   # maternal vaccine effectiveness
  
  if ("meff" %in% names(MASTER_CONTROLS)){meff = MASTER_CONTROLS$meff}
  if ("mcov" %in% names(MASTER_CONTROLS)){mcov = MASTER_CONTROLS$mcov}
  
  
} else if (maternal_vaccine_on == "N"){
  mcov = 0
  meff = 0
  
} else if (maternal_vaccine_on == "Y_random_cov"){
  mcov = rnorm(n=1,mean=0.9743,sd=0.0027)
  meff = 0.75
} else if (maternal_vaccine_on == "Y_lower_cov_tetanus_sufficient"){
  mcov = 0.8482 
  meff = 0.75
} else if (maternal_vaccine_on == "Y_lower_cov_ANC"){
  mcov = 0.8955
  meff = 0.75
} else if (maternal_vaccine_on == "Y_lower_cov_introduction"){
  mcov = 0.70
  meff = 0.75
  
} else if (maternal_vaccine_on == "Y_lower_eff_flu"){
  mcov = 0.9743 
  meff = 0.5
} else if (maternal_vaccine_on == "Y_higher_eff_pertut"){
  mcov = 0.9743 
  meff = 0.9
  
} else{
  stop('Please indicate whether the maternal vaccine is on or off')
}
#NB: 2019 assumptions were mcov=0.95 and meff=0.5

# maternal waning as an exponential
#Careful this is a monthly rate
if (mat_waning == "standard"){
  wan = 0.2520  # standard fit to average of all serotypes waning
} else if (mat_waning == "slower"){
  wan = 0.215467019  # slower waning
} else if (mat_waning == "faster"){
  wan = 0.371026823  # faster waning
}else{
  warning("mode of maternal waning not selected, assuming standard mat_waning")
  wan = 0.4405  # standard fit to average of all serotypes waning
}

#blunting if long-term (crude work around for sensitivity analysis)
if (blunting_long>0 & maternal_vaccine_on == "Y"){
  e=e*(1-blunting_long*mcov)
}


###### (2/4) intialise classes
S_inital=V1_inital=V2_inital=V3_inital=Is_inital=Iv1_inital=Iv2_inital=Iv3_inital=(rep(0,num_age_groups))
Sm_inital=V1m_inital=V2m_inital=V3m_inital=Ism_inital=Iv1m_inital=Iv2m_inital=Iv3m_inital=(rep(0,num_age_groups))


###### (3/4) distribute infections among vaccination classes
for (i in 1:num_age_groups){ #across age classes
  # Step One: split into infected and susceptible
  S_inital[i]=(1-inital_infection[i])*P_inital[i]
  Is_inital[i]=inital_infection[i]*P_inital[i]
  
  # Step Two: distribute across PCV classes (dependent on age!)
  if (i >= PCV_start_bracket[1]){
    if (i >= PCV_start_bracket[1] & i < PCV_start_bracket[2]){
      V1_inital[i]=S_inital[i]*pcvcov[1]
      S_inital[i]=S_inital[i]*(1-pcvcov[1])
    }
    if (i >= PCV_start_bracket[2] & i < PCV_start_bracket[3]){
      V1_inital[i]=S_inital[i]*pcvcov[1]
      S_inital[i]=S_inital[i]*(1-pcvcov[1])
      V2_inital[i]=V1_inital[i]*pcvcov[2]
      V1_inital[i]=V1_inital[i]*(1-pcvcov[2])
    }
    if (i>PCV_start_bracket[3]){
      V1_inital[i]=S_inital[i]*pcvcov[1]
      S_inital[i]=S_inital[i]*(1-pcvcov[1])
      V2_inital[i]=V1_inital[i]*pcvcov[2]
      V1_inital[i]=V1_inital[i]*(1-pcvcov[2])
      V3_inital[i]=V2_inital[i]*pcvcov[3]
      V2_inital[i]=V2_inital[i]*(1-pcvcov[3])
    }
  }
}

# Step Three: distribute across maternal classes
for (i in 1:12){

  mcov_true=mcov*(wan^(i-1))
  
  Sm_inital[i]=mcov_true*S_inital[i]
  S_inital[i]=(1-mcov_true)*S_inital[i]
  
  V1m_inital[i]=mcov_true*V1_inital[i]
  V1_inital[i]=(1-mcov_true)*V1_inital[i]
  
  V2m_inital[i]=mcov_true*V2_inital[i]
  V2_inital[i]=(1-mcov_true)*V2_inital[i]
  
  V3m_inital[i]=mcov_true*V3_inital[i]
  V3_inital[i]=(1-mcov_true)*V3_inital[i]
  
  Ism_inital[i]=mcov_true*Is_inital[i]
  Is_inital[i]=(1-mcov_true)*Is_inital[i]
  
  Iv1m_inital[i]=mcov_true*Iv1_inital[i]
  Iv1_inital[i]=(1-mcov_true)*Iv1_inital[i]
  
  Iv2m_inital[i]=mcov_true*Iv2_inital[i]
  Iv2_inital[i]=(1-mcov_true)*Iv2_inital[i]
  
  Iv3m_inital[i]=mcov_true*Iv3_inital[i]
  Iv3_inital[i]=(1-mcov_true)*Iv3_inital[i]
  
}

###### (4/4) construct silly array that ODE solver requires
state=c(S_inital,V1_inital,V2_inital,V3_inital,Is_inital,Iv1_inital,Iv2_inital,Iv3_inital,
        Sm_inital,V1m_inital,V2m_inital,V3m_inital,Ism_inital,Iv1m_inital,Iv2m_inital,Iv3m_inital,
        rep(0,num_age_groups))

