###################################################################
### This program models a maternal vaccine against pneumoccocal ###                                      
### The program was first developed in 2019 in MATLAB           ###
###                                                             ###
### Author: Gizem Mayis Bilgin                                  ###
### Last update: 05/08/2021                                     ###
###################################################################

# clear global environment
rm(list=ls())
setwd("~/PhD/Planning/2021_07_choosing a language/R")


#       (1/7) Importing relevant libraries                 
####################################################################
library(deSolve)
library(dplyr)
#####################################################################



#       (2/7) User Choices                 
####################################################################
maternal_vaccine_on = "N"  # do you want the maternal vaccine on "Y" or off "N"?
blunting = 0  # reduction in PCV efficacy as a result of maternal antibody presence

model_years = 2  # how many years should the model run for?
time_step = (365.25/6) # number of days of time step
num_model_runs = model_years*365.25/time_step

ari = 0.15  # proportion ARI attributable to pneumococcal

num_classes = 17  # number of classes per age group (16+1 to count new cases?)
num_age_groups = 14
#####################################################################



#       (3/7) Toggles for sensitivity analysis                 
####################################################################
#COMEBACK these are all deterministic, could be replaced with probabilistic
pcv_effectiveness = "2019_model"  # options: 2019_model, 2021_model, 2021_random_sample
pcv_coverage = "2017_MICS"  # options: 2017_MICS, 2019_DHS, 2019_DHS_random_sample, 2019_DHS_lower_bound, 2019_DHS_upper_bound
carriage_adj = 1  # adjustment for carriage
recov_adj = 1  # adjustment for recovery
mat_waning = "standard"  # choose between "standard", "slower" and "faster"
#####################################################################



#       (4/7) Setting relevant parameters                 
####################################################################
source(paste(getwd(),"/load_model_param.R",sep=""))
#####################################################################



##      (5/7) Calculating remaining parameters                 
####################################################################
#timing of step
times = seq(0,time_step,by=1)

# beta
# beta is the modification factor on transmission, these values have been fitted with the optimisation function
if (ari == 0.15){
  beta = 3.6262e-04
  } else if (ari == 0.30){
  beta = 7.46e-04
  } else if (ari == 0.50){
  beta = 1.30e-03
}
#####################################################################



##      (6/7) Configuration of initial state                 
####################################################################
inital_infection =(rep(0,num_age_groups))
for (i in 1:3){ 
  inital_infection[i]=ari*0.038
  inital_infection[i+3]=ari*0.086;
  inital_infection[i+6]=ari*0.07
  inital_infection[i+9]=ari*0.07
}
inital_infection[13]=ari*0.034   
inital_infection[14]=ari*0.012

S_inital=V1_inital=V2_inital=V3_inital=Is_inital=Iv1_inital=Iv2_inital=Iv3_inital=(rep(0,num_age_groups))
Sm_inital=V1m_inital=V2m_inital=V3m_inital=Ism_inital=Iv1m_inital=Iv2m_inital=Iv3m_inital=(rep(0,num_age_groups))

for (i in 1:num_age_groups){ #across age classes
  # Step One: split into infected and susceptible
  S_inital[i]=(1-inital_infection[i])*P_inital[i]
  Is_inital[i]=inital_infection[i]*P_inital[i]
  
  # Step Two: distribute across PCV classes (dependent on age!)
  if (i>1){
    if (i == 2){
      V1_inital[i]=S_inital[i]*pcvcov[1]
      S_inital[i]=S_inital[i]*(1-pcvcov[1])
    }
    if (i == 3){
      V1_inital[i]=S_inital[i]*pcvcov[1]
      S_inital[i]=S_inital[i]*(1-pcvcov[1])
      V2_inital[i]=V1_inital[i]*pcvcov[2]
      V1_inital[i]=V1_inital[i]*(1-pcvcov[2])
    }
    if (i>3){
      V1_inital[i]=S_inital[i]*pcvcov[1]
      S_inital[i]=S_inital[i]*(1-pcvcov[1])
      V2_inital[i]=V1_inital[i]*pcvcov[2]
      V1_inital[i]=V1_inital[i]*(1-pcvcov[2])
      V3_inital[i]=V2_inital[i]*pcvcov[3]
      V2_inital[i]=V2_inital[i]*(1-pcvcov[3])
    }
  }
}

#COMEBACK
# correcting for 0-6 months maternal classes
for (i in 1:3){
  Sm_inital[i]=mcov*S_inital[i]
  S_inital[i]=(1-mcov)*S_inital[i]
  
  V1m_inital[i]=mcov*V1_inital[i]
  V1_inital[i]=(1-mcov)*V1_inital[i]
  
  V2m_inital[i]=mcov*V2_inital[i]
  V2_inital[i]=(1-mcov)*V2_inital[i]
  
  V3m_inital[i]=mcov*V3_inital[i]
  V3_inital[i]=(1-mcov)*V3_inital[i]
  
  Ism_inital[i]=mcov*Is_inital[i]
  Is_inital[i]=(1-mcov)*Is_inital[i]
  
  Iv1m_inital[i]=mcov*Iv1_inital[i]
  Iv1_inital[i]=(1-mcov)*Iv1_inital[i]
  
  Iv2m_inital[i]=mcov*Iv2_inital[i]
  Iv2_inital[i]=(1-mcov)*Iv2_inital[i]
  
  Iv3m_inital[i]=mcov*Iv3_inital[i]
  Iv3_inital[i]=(1-mcov)*Iv3_inital[i]

}

#COMEBACK
#making into silly array that ODE solver wants
# state =c(1,2,3)
# for (i in 1:num_age_groups){
#   state=append(state,S_inital[i])
#   state=append(state,V1_inital[i])
#   state=append(state,V2_inital[i])
#   state=append(state, V3_inital[i])
#   state=append(state, Is_inital[i])
#   state=append(state,Iv1_inital[i])
#   state=append(state,Iv2_inital[i])
#   state=append(state,Iv3_inital[i])
#   state=append(state,Sm_inital[i])
#   state=append(state,V1m_inital[i])
#   state=append(state,V2m_inital[i])
#   state=append(state,V3m_inital[i])
#   state=append(state,Ism_inital[i])
#   state=append(state,Iv1m_inital[i])
#   state=append(state,Iv2m_inital[i])
#   state=append(state, Iv3m_inital[i])
#   state=append(state,0) #Incidence initalisor
# }
# state=state[-(1:3)]

state=c(S_inital,V1_inital,V2_inital,V3_inital,Is_inital,Iv1_inital,Iv2_inital,Iv3_inital,
        Sm_inital,V1m_inital,V2m_inital,V3m_inital,Ism_inital,Iv1m_inital,Iv2m_inital,Iv3m_inital,
        rep(0,num_age_groups))


#####################################################################



#      (7/7) Defining ODE               
####################################################################
source(paste(getwd(),"/pneum_ode_function.R",sep=""))
#####################################################################


#      Running ODE !!!               
####################################################################
parameters = c(nu=nu,
               mu=mu,
               recovery=recovery,
               carriage=carriage,
               contact_transmission=contact_transmission,
               e=e,
               meff=meff,
               beta,
               blunting=blunting,
               num_age_groups
)
ode_run = as.data.frame(ode(y=state,times=(seq(0,time_step,by=1)),func=pneumODE,parms=parameters))
#output is time and then a column for all 17*14=238 classes
#tail(ode_run) #print end
#####################################################################



#      Time stepping 
#      This section will run the ode multiple times while aging the cohort over time
####################################################################
#first time step

sol = as.data.frame(ode(y=state,times=(seq(0,time_step,by=1)),func=pneumODE,parms=parameters))
sol_log <- sol
sol_log_unedited <- sol

#COMEBACK might have to configure preval and incid here

#COMEBACK R does not have as elegant a solution as odeextend in MATLAB

for (run_number in 2:num_model_runs){
  
  #selecting bottom row of solution which is time=60 and then 238 classes
  state_working=tail.matrix(sol,1)
  state_working=select(state_working,-time) #remove column with time
  state_working=as.vector(state_working)
  
  #lets reconstruct our matrix
  J=num_age_groups
  S=as.matrix(state_working[1:J])
  V1=as.matrix(state_working[(J+1):(2*J)])
  V2=as.matrix(state_working[(2*J+1):(3*J)])
  V3=as.matrix(state_working[(3*J+1):(4*J)])
  Is=as.matrix(state_working[(4*J+1):(5*J)])
  Iv1=as.matrix(state_working[(5*J+1):(6*J)])
  Iv2=as.matrix(state_working[(6*J+1):(7*J)])
  Iv3=as.matrix(state_working[(7*J+1):(8*J)])
  Sm=as.matrix(state_working[(8*J+1):(9*J)])
  V1m=as.matrix(state_working[(9*J+1):(10*J)])
  V2m=as.matrix(state_working[(10*J+1):(11*J)])
  V3m=as.matrix(state_working[(11*J+1):(12*J)])
  Ism=as.matrix(state_working[(12*J+1):(13*J)])
  Iv1m=as.matrix(state_working[(13*J+1):(14*J)])
  Iv2m=as.matrix(state_working[(14*J+1):(15*J)])
  Iv3m=as.matrix(state_working[(15*J+1):(16*J)])
  
  Incid=as.matrix(state_working[(16*J+1):(17*J)])


  prev_state <- as.data.frame(rbind(S,V1,V2,V3,Is,Iv1,Iv2,Iv3,Sm,V1m,V2m,V3m,Ism,Iv1m,Iv2m,Iv3m))
  row.names(prev_state) <- NULL
  row.names(prev_state) <- c("S","V1","V2","V3","Is","Iv1","Iv2","Iv3","Sm","V1m","V2m","V3m","Ism","Iv1m","Iv2m","Iv3m")
  
  
  #initalise next state
  next_state=prev_state
  
  # first age group (new births)
  #set 0-2 months to zero, then assume all children born susceptible
  next_state[,1] = 0
  next_state[1,1] = nu*(1-mcov)
  next_state[9,1] = nu*mcov
  

  # vaccination rounds #########################################################
  # second age group 
  # apply first dose
  for (i in 1:2){ #for normal classes and then maternally immunised
    for (j in 1:2){ #for S then I
      next_state[1+4*(j-1)+8*(i-1),2]=prev_state[1+4*(j-1)+8*(i-1),1]*(1-pcvcov[1])
      next_state[2+4*(j-1)+8*(i-1),2]=prev_state[1+4*(j-1)+8*(i-1),1]*pcvcov[1]
    }
  }
  
  # third age group 
  # apply second dose
  for (i in 1:16){ #shift all other age classes up
    next_state[i,3]=prev_state[i,2]
  }
  for (i in 1:2){ #for normal classes and then maternally immunised
    for (j in 1:2){ #for S then I
      next_state[2+4*(j-1)+8*(i-1),3]=prev_state[2+4*(j-1)+8*(i-1),2]*(1-pcvcov[2])
      next_state[3+4*(j-1)+8*(i-1),3]=prev_state[2+4*(j-1)+8*(i-1),2]*pcvcov[2]
    }
  }
 
  #fourth age group
  #apply third dose
  for (i in 1:16){ #shift all other age classes up
    next_state[i,4]=prev_state[i,3]
  }
  for (i in 1:2){ #for normal classes and then maternally immunised
    for (j in 1:2){ #for S then I
      next_state[3+4*(j-1)+8*(i-1),4]=prev_state[3+4*(j-1)+8*(i-1),3]*(1-pcvcov[3])
      next_state[4+4*(j-1)+8*(i-1),4]=prev_state[3+4*(j-1)+8*(i-1),3]*pcvcov[3]
    }
  }
  #end vaccination
  
  
  #now just shift all non-PCV immunisation stage age classes up ################
  
  #8 month - 2 year olds
  for (i in 5:12){ next_state[,i]=prev_state[,(i-1)]  }

  #modified stepping for 2-5 and 5+ since not a complete shift up
  next_state[,13]=prev_state[,12]+(17/18)*prev_state[,13]
  next_state[,14]=(1/18)*prev_state[,13]+prev_state[,14]
  
  #waning maternal immunity as an exponential ###################################
  for (i in 1:2){ #S and I
    for (j in 1:4){ #S,V1,V2,V3
      for (k in 1:5) { #age groups
        next_state[j+(i-1)*4,k+1] = next_state[j+(i-1)*4,k+1]+wan*next_state[j+(i-1)*4+8,k+1]
        next_state[j+(i-1)*4+8,k+1] = (1-wan)*next_state[j+(i-1)*4+8,k+1]
      }
      for (k in 6){ #cut off maternal immunity after 12 months
        next_state[j+(i-1)*4,k+1] = next_state[j+(i-1)*4,k+1]+next_state[j+(i-1)*4+8,k+1]
        next_state[j+(i-1)*4+8,k+1] = 0
      }
    }
  }
  
  #deaths #####################################################################
  #deaths proportional to group size, assuming only from over 5
  total=sum(next_state[,14])
  next_state[,14]=next_state[,14]-nu*(next_state[,14]/total)
  
  S_next=next_state[1,]
  V1_next=next_state[2,]
  V2_next=next_state[3,]
  V3_next=next_state[4,]
  Is_next=next_state[5,]
  Iv1_next=next_state[6,]
  Iv2_next=next_state[7,]
  Iv3_next=next_state[8,]
  Sm_next=next_state[9,]
  V1m_next=next_state[10,]
  V2m_next=next_state[11,]
  V3m_next=next_state[12,]
  Ism_next=next_state[13,]
  Iv1m_next=next_state[14,]
  Iv2m_next=next_state[15,]
  Iv3m_next=next_state[16,]
  
  next_state_FINAL=as.numeric(c(S_next,V1_next,V2_next,V3_next,Is_next,Iv1_next,Iv2_next,Iv3_next,
          Sm_next,V1m_next,V2m_next,V3m_next,Ism_next,Iv1m_next,Iv2m_next,Iv3m_next,
          rep(0,num_age_groups)))
  #setting Incid to repeated 0s
  
  sol <- as.data.frame(ode(y=next_state_FINAL,times=(seq(0,time_step,by=1)),func=pneumODE,parms=parameters))
  
  sol[,1]=sol[,1]+60*(run_number-1) #make times correct
  
  
  sol_log=head(sol_log,-1) #remove last entry from sol_log
  sol_log <- rbind(sol_log,sol)
  
  sol_log_unedited <- rbind(sol_log_unedited,sol)
    
}

### INCIDENCE CALCULATIONS 
incidence_log <- sol_log_unedited %>% select(1, ((num_classes-1)*num_age_groups+2):(num_classes*num_age_groups+1)) #select(1, 226:239) # selecting [1,225:239]

#select end points of 2 month intervals 
incidence_log <- incidence_log %>% filter (time %% 60 == 0, rowSums(incidence_log) != time)

incidence_1000PY <-incidence_log[,-1]
for (i in 1:num_age_groups){
  incidence_1000PY[,i] = 1000*6*incidence_1000PY[,i]*(1/P_inital[i])
}

incidence_1000PY  <- incidence_1000PY %>%
  mutate(month_0_6 = 225+226+227)

incidence_1000PY$month_0_6 <- incidence_1000PY[,1] + incidence_1000PY[,2] + incidence_1000PY[,3]
incidence_1000PY$month_6_12 <- incidence_1000PY[,4] + incidence_1000PY[,5] + incidence_1000PY[,6]

tail(incidence_1000PY$month_0_6,1)
tail(incidence_1000PY$month_6_12,1)


#####################################################################

