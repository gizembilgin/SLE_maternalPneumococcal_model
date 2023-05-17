
### This script contains the differential equations for the pneumococcal transmission model

pneumODE <- function(t, state, parameters){
  require(deSolve)
  
  with(as.list(c(state,parameters)),{
    
    J=num_age_groups
    
    S=state[1:J]
    V1=state[(J+1):(2*J)]
    V2=state[(2*J+1):(3*J)]
    V3=state[(3*J+1):(4*J)]
    Is=state[(4*J+1):(5*J)]
    Iv1=state[(5*J+1):(6*J)]
    Iv2=state[(6*J+1):(7*J)]
    Iv3=state[(7*J+1):(8*J)]
    Sm=state[(8*J+1):(9*J)]
    V1m=state[(9*J+1):(10*J)]
    V2m=state[(10*J+1):(11*J)]
    V3m=state[(11*J+1):(12*J)]
    Ism=state[(12*J+1):(13*J)]
    Iv1m=state[(13*J+1):(14*J)]
    Iv2m=state[(14*J+1):(15*J)]
    Iv3m=state[(15*J+1):(16*J)]
    
    
    dS <- numeric(length=num_age_groups)
    dV1 <- numeric(length=num_age_groups)
    dV2 <- numeric(length=num_age_groups)
    dV3 <- numeric(length=num_age_groups)
    dIs <- numeric(length=num_age_groups)
    dIv1 <- numeric(length=num_age_groups)
    dIv2 <- numeric(length=num_age_groups)
    dIv3 <- numeric(length=num_age_groups)
    
    dSm <- numeric(length=num_age_groups)
    dV1m <- numeric(length=num_age_groups)
    dV2m <- numeric(length=num_age_groups)
    dV3m <- numeric(length=num_age_groups)
    dIsm <- numeric(length=num_age_groups)
    dIv1m <- numeric(length=num_age_groups)
    dIv2m <- numeric(length=num_age_groups)
    dIv3m <- numeric(length=num_age_groups)
    
    dIncidence <-numeric(length=num_age_groups)
    
    trans =(rep(0,num_age_groups))
    
    for (i in 1:num_age_groups){
      
      #total_susceptible=S[i]+V1[i]+V2[i]+V3[i]+Sm[i]+V1m[i]+V2m[i]+V3m[i]
      #total_infected=Is[i]+Iv1[i]+Iv2[i]+Iv3[i]+Ism[i]+Iv1m[i]+Iv2m[i]+Iv3m[i]
      #total=total_susceptible+total_infected
      for (j in 1:num_age_groups){
        total_susceptible=S[j]+V1[j]+V2[j]+V3[j]+Sm[j]+V1m[j]+V2m[j]+V3m[j]
        total_infected=Is[j]+Iv1[j]+Iv2[j]+Iv3[j]+Ism[j]+Iv1m[j]+Iv2m[j]+Iv3m[j]
        total=total_susceptible+total_infected
        
        trans[i]=trans[i]+contact_transmission[i,j]*beta*(carriage[j]*total_susceptible+total_infected)/(total)
      }
      trans[i]=max(min(1,trans[i]),0)
      
      
      dS[i]=-trans[i]*S[i]+recovery[i]*Is[i]
      dV1[i]=-trans[i]*V1[i]*(1-e[1])+recovery[i]*Iv1[i]
      dV2[i]=-trans[i]*V2[i]*(1-e[2])+recovery[i]*Iv2[i]
      dV3[i]=-trans[i]*V3[i]*(1-e[3])+recovery[i]*Iv3[i]
      dIs[i]=trans[i]*S[i]-recovery[i]*Is[i]
      dIv1[i]=trans[i]*V1[i]*(1-e[1])-recovery[i]*Iv1[i]
      dIv2[i]=trans[i]*V2[i]*(1-e[2])-recovery[i]*Iv2[i]
      dIv3[i]=trans[i]*V3[i]*(1-e[3])-recovery[i]*Iv3[i]
      
      dSm[i]=-trans[i]*Sm[i]*(1-meff)+recovery[i]*Ism[i]
      dV1m[i]=-trans[i]*V1m[i]*(1-e[1]*(1-blunting_temp))*(1-meff)+recovery[i]*Iv1m[i]
      dV2m[i]=-trans[i]*V2m[i]*(1-e[2]*(1-blunting_temp))*(1-meff)+recovery[i]*Iv2m[i]
      dV3m[i]=-trans[i]*V3m[i]*(1-e[3]*(1-blunting_temp))*(1-meff)+recovery[i]*Iv3m[i]
      dIsm[i]=trans[i]*Sm[i]*(1-meff)-recovery[i]*Ism[i]
      dIv1m[i]=trans[i]*V1m[i]*(1-e[1]*(1-blunting_temp))*(1-meff)-recovery[i]*Iv1m[i]
      dIv2m[i]=trans[i]*V2m[i]*(1-e[2]*(1-blunting_temp))*(1-meff)-recovery[i]*Iv2m[i]
      dIv3m[i]=trans[i]*V3m[i]*(1-e[3]*(1-blunting_temp))*(1-meff)-recovery[i]*Iv3m [i]
      
      dIncidence[i]=trans[i]*(S[i]+Sm[i]*(1-meff)+V1[i]*(1-e[1])+V1m[i]*(1-meff)*(1-e[1]*(1-blunting_temp))+
                                V2[i]*(1-e[2])+V2m[i]*(1-meff)*(1-e[2]*(1-blunting_temp))+V3[i]*(1-e[3])+V3m[i]*(1-meff)*(1-e[3]*(1-blunting_temp)))
      
    }
    
    list(c(dS,dV1,dV2,dV3,dIs,dIv1,dIv2,dIv3,
           dSm,dV1m,dV2m,dV3m,dIsm,dIv1m,dIv2m,dIv3m,
           dIncidence))  
  })
}