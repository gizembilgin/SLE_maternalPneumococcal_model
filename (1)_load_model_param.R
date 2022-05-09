
#       (0/5) Generic variables                
################################################################################
num_classes = 17  # number of classes per age group (16+1 for incidence)
num_age_groups = 17
age_group_titles <- c('0-1 months','1-2 months','2-3 months','3-4 months','4-5 months','5-6 months','6-8 months','8-10 months',
                      '10-12 months','12-14 months','14-16 months','16-18 months',
                      '18-20 months','20-22 months','22-24 months','2-5 years','5+ years')

time_step = (365.25/12) # number of days of time step
num_model_increments = model_years*365.25/time_step
times = seq(0,time_step,by=1)

################################################################################



#       (1/5) Loading study setting                
################################################################################
### (A) Initalise age groups
# configuring to demographic of Sierra Leone
N = 8297882           # total pop size - 2021 projection from Statistics Sierra Leone, 2015 Population and Housing Census ('Thematic report on population projections')
life_expectancy = 49.5  # life expectancy from latest census 2015 Population and Housing Census('Thematic report on life tables')

#setting age structure
P_inital =(rep(0,num_age_groups))
if (pop_distribution == "uniform"){ #uniform distribution according to standard life expectancy
  for (i in 1:6){P_inital[i]=N/(life_expectancy*12)}
  for (i in 7:15){P_inital[i]=N/(life_expectancy*6)}
  P_inital[16]=N*3/life_expectancy
  P_inital[17]=N*(life_expectancy-5)/life_expectancy
  
} else if (pop_distribution=="census") { #age distribution from 2019 DHS survey table 2.8 (aligns with 2015 Population and Housing Census)
  for (i in 1:6){P_inital[i]=(0.029*N)/12} 
  for (i in 7:15){P_inital[i]=(0.029*N)/6} 
  P_inital[16]=N*0.087 # 2-5 years
  P_inital[17]=N*0.855 #5+ age group
}
if (sum(P_inital) != N){stop('population not configured correctly!')}

#setting birth rate
nu=P_inital[1]*pop_growth_rate   #set birth rate (per month) to inital number in 0-1 months

mu_import<-read.csv("death_rates.csv") #age distribution of deaths
if (death_distribution == "oldest_only"){  #death rate set to birth rate
  mu = rep(0,num_age_groups)
  mu[num_age_groups] = nu
} else if (death_distribution == "2019_DHS"){
  mu=nu*(mu_import[,3]/100)
}


### (B) Initalise PCV coverage
# pcv coverage of doses
if (pcv_coverage == "2017_MICS"){
  true_cov = c(92.33, 79.05, 57.26)  # data used in 2019 paper
  
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
carriage_import<-read.csv("carriage_by_age.csv") #data frame of carriage by age

if (carriage_setting == "2019_model"){
  carriage=carriage_import[,2]
} else if (carriage_setting == "2021_model"){
  carriage=carriage_import[,3] #TINKER ON
} else if (carriage_setting == "2022_model"){
  carriage=carriage_import[,4] #TINKER ON
}

carriage=carriage_adj*carriage #set up for sensitivity analysis


### (B) Recovery
recovery_import<-read.csv("recovery_by_age.csv") #data frame of recovery time by age

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
if (contact_setting == "2019_model"){
  contact_matrix <- read.csv("contact_matrix_2019.csv",header=TRUE)
} else if (contact_setting == "2021_model"){
  contact_matrix <- read.csv("contact_matrix.csv",header=TRUE)
}
contact_matrix = contact_matrix[-1]

if (covid_sensitivity == "on"){
  contact_modification <- read.csv("contact_matrix_covid.csv",header=TRUE) #stores % reduction of contact
  contact_matrix= contact_matrix*(1-contact_modification[-1])
}



### (D) Transmission probability from contact
if (transmission_setting == "2019_model"){
  tranmission_probab <- read.csv("transmission_matrix_2019.csv",header=TRUE)
} else if (transmission_setting == "2021_model"){
  tranmission_probab <- read.csv("transmission_matrix_2021.csv",header=TRUE)
}


contact_transmission=contact_matrix*tranmission_probab[-1]

if (sum(contact_transmission[,]>1)>0){
  warning("contact transmission matrix configured incorrectly")
}
################################################################################


#       (4/5) Loading prevalence of initial infection            
################################################################################
#(A) importing DHS ARI prevalence data
#importing DHS data for prevalence of ARI in children < 60 months
ARI_import<-read.csv("ARI_prevalence.csv") #data frame of recovery time by age

if (ARI_setting == "DHS_2013"){
  ARI_import<-ARI_import[,c(1,2)]
} else if (ARI_setting == "DHS_2019"){
  ARI_import<-ARI_import[,c(1,3)]
}

#(B) multiplying ARI prevalence by % ARI attributable to pneumoccocal
inital_infection =(rep(0,num_age_groups))
for (i in 1:3){ 
  inital_infection[i]=ari*ARI_import[1,2]/100
  inital_infection[i+3]=ari*ARI_import[1,2]/100
  inital_infection[i+6]=ari*ARI_import[2,2]/100
  inital_infection[i+9]=ari*ARI_import[3,2]/100
  inital_infection[i+12]=ari*ARI_import[3,2]/100
}
inital_infection[16]=ari*mean(c(ARI_import[4,2],ARI_import[5,2],ARI_import[6,2]),na.rm=FALSE)/100 
inital_infection[17]=ari*ARI_import[7,2]/100 #COMEBACK
inital_infection
################################################################################


#       (5/5) Disease burden           
################################################################################
