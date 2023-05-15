
#       (0/5) Generic variables                
################################################################################
num_classes = 17  # number of classes per age group (16+1 for incidence)
age_group_titles <- c('0 months','1 months','2 months','3 months','4 months','5 months',
                      '6 months','7 months','8 months','9 months','10 months','11 months',
                      '12 months','13 months','14 months','15 months','16 months','17 months',
                      '18 months','19 months','20 months','21 months','22 months','23 months',
                      '2-5 years','5+ years')
num_age_groups = length(age_group_titles)

num_model_increments = model_years*365.25/time_step
times = seq(0,time_step,by=1)

################################################################################



#       (1/5) Loading study setting                
################################################################################
### (A) Initalise age groups
# configuring to demographic of Sierra Leone
N = 8297882             # total pop size - 2021 projection from Statistics Sierra Leone, 2015 Population and Housing Census ('Thematic report on population projections')
life_expectancy = 49.5  # life expectancy from latest census 2015 Population and Housing Census('Thematic report on life tables')

#setting age structure
P_inital =(rep(0,num_age_groups))
if (pop_distribution == "uniform"){ #uniform distribution according to standard life expectancy
  for (i in 1:24){P_inital[i]=N/(life_expectancy*12)}
  P_inital[num_age_groups-1]=N*3/life_expectancy
  P_inital[num_age_groups]=N*(life_expectancy-5)/life_expectancy
  
} else if (pop_distribution=="census") { #age distribution from 2019 DHS survey table 2.8 (aligns with 2015 Population and Housing Census)
  for (i in 1:24){P_inital[i]=(0.058*N)/24} 
  P_inital[num_age_groups-1]=N*0.087 # 2-5 years
  P_inital[num_age_groups]=N*0.855 #5+ age group
}
if (round(sum(P_inital)) != N){stop('population not configured correctly!')}

#setting birth rate
nu=P_inital[1]*pop_growth_rate   #set birth rate (per month) to inital number in 0-1 months

if (death_distribution == "oldest_only"){  #death rate set to birth rate
  mu = rep(0,num_age_groups)
  mu[num_age_groups] = nu
} else if (death_distribution == "2019_DHS"){
  mu_import<-read.csv("1_inputs/death_rates.csv") #age distribution of deaths
  mu=nu*(mu_import[,3]/100)
}


### (B) Initalise PCV coverage
# pcv coverage of doses
if (pcv_coverage == "2017_MICS"){
  true_cov = c(92.33, 79.05, 57.26)  # data used in 2019 Honours Thesis
  
}else if (pcv_coverage == "2019_DHS"){ # from analysis of 2019 DHS data
  true_cov = c(94.44, 87.31, 80.79)
  
}else if (pcv_coverage == "2019_DHS_random_sample"){
  true_cov = c(max(min(rnorm(1,94.44,0.69),100),0),
               max(min(rnorm(1,87.31,0.86),100),0),
               max(min(rnorm(1,80.79,1.12),100),0))
  
}else if (pcv_coverage == "2019_DHS_lower_bound"){
  true_cov = c(93.07,85.43,78.60)
}else if (pcv_coverage == "2019_DHS_upper_bound"){
  true_cov = c(95.79,89.20,82.98)
  
}else if (pcv_coverage == "sensitivity_DTP"){
  true_cov = c(95,93,91)
}else if (pcv_coverage == "sensitivity_MCV"){
  true_cov = c(87,67,47)
  
}else{
  warning("no pcv coverage selected, assuming 2019 DHS data")
  true_cov = c(94.44, 87.31, 80.79)
}

if (covid_sensitivity == "on"){
  true_cov = true_cov + c(-3.5,-5,-7)
}


pcvcov=c((true_cov[1]/100),
         (true_cov[2]/true_cov[1]),
         (true_cov[3]/true_cov[2])) #proportion of first dose that get second etc.

if (pcv_coverage == "absent"){pcvcov = c(0,0,0)}
################################################################################


#       (2/5) Loading PCV characteristics               
################################################################################


# pcv effectiveness as an array per # of doses
if (pcv_effectiveness == "2019_model"){ #conservative estimate, Cochrane review using primarily PCV7 data
  e = c(0.24, 0.5, 0.58)
} else if (pcv_effectiveness == "2021_model"){ #increasing effectiveness due to additional serotypes in PCV13
  e = c(0.8*0.806*0.5,
        0.8*0.806*0.8,
        0.8*0.806)
} else if (pcv_effectiveness == "2021_random_sample"){ 
  #very crude random sampling method that assumes uniform distribution on uncertainty - we actually know the distributions are skewed
  e_max=max(min(rnorm(1,0.8,0.0816),1),0)*max(min(rnorm(1,0.806,0.0617),1),0)
  e = c(e_max*0.5,
        e_max*0.8,
        e_max)
}else if (pcv_effectiveness == "2021_upper_serotype_cov"){
  e = c(0.8*0.905*0.5,
        0.8*0.905*0.8,
        0.8*0.905)
}else if (pcv_effectiveness == "2021_lower_serotype_cov"){
  e = c(0.8*0.663*0.5,
        0.8*0.663*0.8,
        0.8*0.663)
}else{
  warning("pcv effectiveness not selected, assuming standard 2021_model pcv_effectiveness")
  e = c(0.8*0.806*0.5,
        0.8*0.806*0.8,
        0.8*0.806)
}
################################################################################



#       (3/5) Loading disease characteristics              
################################################################################
# disease characteristics include: carriage, recovery, transmission probability and contact 

### (A) Carriage
carriage_import<-read.csv("1_inputs/carriage_by_age.csv") #data frame of carriage by age
carriage = carriage_import$carriage
carriage=carriage_adj*carriage #set up for sensitivity analysis


### (B) Recovery
recovery_import<-read.csv("1_inputs/recovery_by_age.csv") #data frame of recovery time by age

if (recov_setting == "2019_model"){
  recovery=recovery_import[,2]
} else if (recov_setting == "2021_model"){
  recovery=recovery_import[,3]
} else if (recov_setting == "2021_lower"){ #note that we were unable to identify the probability distribution used in this paper
  recovery=recovery_import[,4]
} else if (recov_setting == "2021_upper"){
  recovery=recovery_import[,5]
}
recovery=recov_adj*recovery #set up for sensitivity analysis


### (C) Contact matrix
contact_matrix <- read.csv("1_inputs/contact_matrix_v3.csv",header=TRUE)
contact_matrix = contact_matrix[-1]

if (covid_sensitivity == "on"){
  contact_modification <- read.csv("1_inputs/contact_matrix_covid.csv",header=TRUE) #stores % reduction of contact
  contact_matrix= contact_matrix*(1-contact_modification[-1])
}



### (D) Transmission probability from contact
tranmission_probab <- read.csv("1_inputs/transmission_matrix_2022.csv",header=TRUE)

contact_transmission=contact_matrix*tranmission_probab[-1]

if (sum(contact_transmission[,]>1)>0){
  warning("contact transmission matrix configured incorrectly")
}
################################################################################


#       (4/5) Loading prevalence of initial infection            
################################################################################
#(A) importing DHS ARI prevalence data
#importing DHS data for prevalence of ARI in children < 60 months
ARI_import<-read.csv("1_inputs/ARI_prevalence.csv") #data frame of recovery time by age

if (ARI_setting == "DHS_2013"){
  ARI_import<-ARI_import[,c(1,2)]
} else if (ARI_setting == "DHS_2019"){
  ARI_import<-ARI_import[,c(1,3)]
}

#(B) multiplying ARI prevalence by % ARI attributable to pneumoccocal
inital_infection =(rep(0,num_age_groups))
inital_infection[c(1:6)] = ari*ARI_import[1,2]/100
inital_infection[c(7:12)]=ari*ARI_import[2,2]/100
inital_infection[c(13:24)]=ari*ARI_import[3,2]/100
inital_infection[25]=ari*mean(c(ARI_import[4,2],ARI_import[5,2],ARI_import[6,2]),na.rm=FALSE)/100 
inital_infection[26]=ari*ARI_import[7,2]/100 #COMEBACK
inital_infection
################################################################################


#       (5/5) Disease burden           
################################################################################
