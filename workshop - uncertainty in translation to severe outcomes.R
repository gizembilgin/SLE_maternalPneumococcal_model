
### Step One: create dataset with mean, LB, UB #################################
point <- read.csv("1_inputs/health_outcomes_num_SL.csv",header=TRUE) %>%
  rename(mean = num) %>%
  select(-percentage)
LB <- read.csv("1_inputs/health_outcomes_num_SL_LB.csv",header=TRUE) %>%
  rename(LB = num) %>%
  select(-percentage)
UB <- read.csv("1_inputs/health_outcomes_num_SL_UB.csv",header=TRUE) %>%
  rename(UB = num) %>%
  select(-percentage)

workshop = point %>%
  left_join(LB,by = join_by(category, measure)) %>%
  left_join(UB,by = join_by(category, measure))

ggplot(workshop) +
  geom_pointrange(aes(x=mean,y=measure,
                      xmin = LB, xmax = UB)) +
  facet_grid(category~.)

#CI not even, therefore lognormal or gamma fit
#_______________________________________________________________________________



### Step Two: initialise fitting function #######################################
fit_lognormal <- function(mean,LB,UB){
  
  minimise_this_lognormal <- function(sd) {
    a = log(mean^2/sqrt(mean^2+sd^2))
    b = sqrt(log(1+(sd^2/mean^2)))
    
    Z= rlnorm(10000000, meanlog = a,sdlog = b)
    LB_estimate <- as.numeric(quantile(Z,.025,na.rm=TRUE))
    UB_estimate <- as.numeric(quantile(Z,.975,na.rm=TRUE))
    mean_estimate <-  mean(Z)
    
    return((LB - LB_estimate)^2 + 
             (mean - mean_estimate)^2 + 
             (UB - UB_estimate)^2)
  }
  
  sd_est = optimize(minimise_this_lognormal, interval=c(0,mean*2))
  sd_est = sd_est$minimum
  
  mean_sq = mean^2
  sd_sq = sd_est^2
  a = log(mean_sq/sqrt(mean_sq+sd_sq))
  b = log(1+sd_sq/mean_sq)
  
  return(data.frame(lognorm_a=a,lognorm_b=b))
}


fit_gamma <- function(mean,LB,UB){
  
  minimise_this_gamma <- function(param) {
    shape=param[1]
    scale=param[2]
    
    Z= rgamma(10000000, shape, scale=scale)
    LB_estimate <- as.numeric(quantile(Z,.025,na.rm=TRUE))
    UB_estimate <- as.numeric(quantile(Z,.975,na.rm=TRUE))
    mean_estimate <-  mean(Z)
    
    return((LB - LB_estimate)^2 + 
             (mean - mean_estimate)^2 + 
             (UB - UB_estimate)^2)
  }
  
  X_estimate = optim(c(1,1),minimise_this_gamma)
  shape_estimate = X_estimate$par[1]
  scale_estimate = X_estimate$par[2]
  
  return(data.frame(gamma_shape=shape_estimate,gamma_scale=scale_estimate))
}
#_______________________________________________________________________________



### Step Three: fit distributions ##############################################
lognorm_param = mapply(fit_lognormal, workshop$mean, workshop$LB, workshop$UB)
workshop = cbind(workshop,t(lognorm_param)) 

gamma_param = mapply(fit_gamma, workshop$mean, workshop$LB, workshop$UB)
workshop = cbind(workshop,t(gamma_param))
#_______________________________________________________________________________



### Step Four: check distributions ##############################################
sampled_lognorm = mapply(rlnorm,10000000,workshop$lognorm_a , workshop$lognorm_b)
sampled_gamma = mapply(rgamma,10000000,workshop$gamma_shape, 1/data.frame(workshop$gamma_scale))


num = 1
workshop[num,c("mean","LB","UB")]
plot(density(sampled_lognorm[,num])); quantile(sampled_lognorm[,num],probs=c(0.05,0.5,0.95)); mean(sampled_lognorm[,num])
plot(density(sampled_gamma[,num])); quantile(sampled_gamma[,num],probs=c(0.05,0.5,0.95)); mean(sampled_gamma[,num])
#_______________________________________________________________________________



### Step Five: save best fitted distribution ###################################
severe_outcome_distributions <- workshop %>% select(-lognorm_a,-lognorm_b)
save(severe_outcome_distributions, file = "1_inputs/severe_outcome_distributions.Rdata")
#_______________________________________________________________________________