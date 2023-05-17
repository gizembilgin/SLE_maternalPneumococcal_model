#      This section will run the ode multiple times while aging the cohort over time
#      This script is an alternate version of (4)_time_step to allow aging per day, rather than per months, to address
#      sensitivity analysis requested during peeer review of paper 1

SOME = 1/ageing_step #COMEBACK - if you want to merge with normal (4) time step
parameters = c(nu=nu,
               mu=mu,
               recovery=recovery,
               carriage=carriage,
               contact_transmission=contact_transmission,
               e=e,
               meff=meff,
               beta,
               blunting_temp=blunting_temp,
               num_age_groups
)

#first time step
sol = as.data.frame(ode(y=state,times=(seq(0,time_step,by=1)),func=pneumODE,parms=parameters))

#initalising solution log
sol_log <- sol
sol_log_unedited <- sol


for (increments_number in 2:(model_years*365)){

  time_now = increments_number*time_step
  
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
  next_state[,1] = 0 + (1-SOME)*prev_state[,1]
  next_state[1,1] = nu*(1-mcov)*SOME + (1-SOME)*prev_state[1,1] #S
  next_state[9,1] = nu*mcov*SOME + (1-SOME)*prev_state[9,1]     #Sm
    
    
    # vaccination rounds #########################################################
    # second age group 
    # apply first dose
    for (i in 1:2){ #for normal classes and then maternally immunised
      for (j in 1:2){ #for S then I
        next_state[1+4*(j-1)+8*(i-1),PCV_start_bracket[1]]=prev_state[1+4*(j-1)+8*(i-1),PCV_start_bracket[1]-1]*(1-pcvcov[1])*SOME + (1-SOME)*prev_state[1+4*(j-1)+8*(i-1),PCV_start_bracket[1]]
        next_state[2+4*(j-1)+8*(i-1),PCV_start_bracket[1]]=prev_state[1+4*(j-1)+8*(i-1),PCV_start_bracket[1]-1]*pcvcov[1]*SOME + (1-SOME)*prev_state[2+4*(j-1)+8*(i-1),PCV_start_bracket[1]]
      }
    }
    
    # third age group 
    # apply second dose
    for (i in 1:16){ #shift all other age classes up
      next_state[i,PCV_start_bracket[2]]=prev_state[i,PCV_start_bracket[2]-1]*SOME + (1-SOME)*prev_state[i,PCV_start_bracket[2]]
    }
    for (i in 1:2){ #for normal classes and then maternally immunised
      for (j in 1:2){ #for S then I
        next_state[2+4*(j-1)+8*(i-1),PCV_start_bracket[2]]=prev_state[2+4*(j-1)+8*(i-1),PCV_start_bracket[2]-1]*(1-pcvcov[2])*SOME + (1-SOME)*prev_state[2+4*(j-1)+8*(i-1),PCV_start_bracket[2]]
        next_state[3+4*(j-1)+8*(i-1),PCV_start_bracket[2]]=prev_state[2+4*(j-1)+8*(i-1),PCV_start_bracket[2]-1]*pcvcov[2]*SOME + (1-SOME)*prev_state[3+4*(j-1)+8*(i-1),PCV_start_bracket[2]]
      }
    }
    
    #fourth age group
    #apply third dose
    for (i in 1:16){ #shift all other age classes up
      next_state[i,PCV_start_bracket[3]]=prev_state[i,PCV_start_bracket[3]-1]*SOME + (1-SOME)*prev_state[i,PCV_start_bracket[3]]
    }
    for (i in 1:2){ #for normal classes and then maternally immunised
      for (j in 1:2){ #for S then I
        next_state[3+4*(j-1)+8*(i-1),PCV_start_bracket[3]]=prev_state[3+4*(j-1)+8*(i-1),PCV_start_bracket[3]-1]*(1-pcvcov[3])*SOME + (1-SOME)*prev_state[3+4*(j-1)+8*(i-1),PCV_start_bracket[3]]
        next_state[4+4*(j-1)+8*(i-1),PCV_start_bracket[3]]=prev_state[3+4*(j-1)+8*(i-1),PCV_start_bracket[3]-1]*pcvcov[3]*SOME + (1-SOME)*prev_state[4+4*(j-1)+8*(i-1),PCV_start_bracket[3]]
      }
    }
    #end vaccination
    
    
    #now just shift all non-PCV immunisation stage age classes up ################
    
    #8 month - 2 year olds
    list_included = c(2:(num_age_groups-2))
    list_included = list_included[!(list_included) %in% PCV_start_bracket]
    for (i in list_included){ next_state[,i]=prev_state[,(i-1)]*SOME + (1-SOME)*prev_state[,i]}
    
    #modified stepping for 2-5 and 5+ since not a complete shift up
    next_state[,num_age_groups-1]=prev_state[,num_age_groups-2]*SOME+(35/36)*prev_state[,num_age_groups-1]*SOME + (1-SOME)*prev_state[,num_age_groups-1]
    next_state[,num_age_groups]=(1/36)*prev_state[,num_age_groups-1]*SOME+prev_state[,num_age_groups]*SOME + (1-SOME)*prev_state[,num_age_groups]
    
    #deaths #####################################################################
    #deaths proportional to group size, assuming only from over 5
    for (i in 1:num_age_groups){
      total=sum(next_state[,i])
      next_state[,i]=next_state[,i]-mu[i]*(next_state[,i]/total)*SOME
    }
    
   
  
  
  if (time_now %% 30 == 0){ #make sure maternal waning stays at monthly (since param calculated this way)
    #waning maternal immunity as an exponential ###################################
    for (i in 1:2){ #S and I
      for (j in 1:4){ #S,V1,V2,V3
        for (k in 1:11) { #age groups
          next_state[j+(i-1)*4,k+1] = next_state[j+(i-1)*4,k+1]+wan*next_state[j+(i-1)*4+8,k+1]
          next_state[j+(i-1)*4+8,k+1] = (1-wan)*next_state[j+(i-1)*4+8,k+1]
        }
        for (k in 12){ #cut off maternal immunity after 12 months
          next_state[j+(i-1)*4,k+1] = next_state[j+(i-1)*4,k+1]+next_state[j+(i-1)*4+8,k+1]
          next_state[j+(i-1)*4+8,k+1] = 0
        }
      }
    }
  }
  
  if (round(sum(next_state)) != round(sum(prev_state))){stop('population fluctating! next_state !=prev_state')}
  if (round(sum(next_state)) != N){stop('population fluctating! pop != next_state')}
  
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
  
  sol <- as.data.frame(ode(y=next_state_FINAL,times=(seq(0,round(time_step),by=1)),func=pneumODE,parms=parameters))
  
  sol[,1]=sol[,1]+round(time_step)*(increments_number-1) #make times correct
  
  
  sol_log=head(sol_log,-1) #remove last entry from sol_log
  sol_log <- rbind(sol_log,sol)
  
  sol_log_unedited <- rbind(sol_log_unedited,sol)
}  


### INCIDENCE CALCULATIONS 
incidence_log <- sol_log_unedited %>% select(1, ((num_classes-1)*num_age_groups+2):(num_classes*num_age_groups+1)) 
#select(1, 226:239) # selecting [1,225:239] where 1 is time

#select end points of 2 month intervals 
incidence_log <- incidence_log %>% filter (time %% round(time_step) == 0, rowSums(incidence_log) != time)

incidence_1000PY <- incidence_log[,-1]
for (i in 1:num_age_groups){
  incidence_1000PY[,i] = 1000*num_model_increments*incidence_1000PY[,i]*(1/P_inital[i])
}

### PREVALENCE CALCULATIONS
prevalence=rep(0,num_age_groups)
for (i in 1:num_age_groups){
  total_infected = Is[1,i]+Iv1[1,i]+Iv2[1,i]+Iv3[1,i]+Ism[1,i]+Iv1m[1,i]+Iv2m[1,i]+Iv3m[1,i]
  prevalence[i]=100*total_infected/P_inital[i]
  
}


