
### This script fits a gamma distribution to estimates from previous literature

minimise_this_gamma <- function(param) {
  shape=param[1]
  scale=param[2]
  
  median = median
  LQR = LQR
  UQR = UQR

  Z= rgamma(10000000, shape, scale=scale)

  (LQR - quantile(Z,na.rm=TRUE)[2])^2 + (median - quantile(Z,na.rm=TRUE)[3])^2 + (UQR - quantile(Z,na.rm=TRUE)[4])^2
  #(LQR - quantile(Z,na.rm=TRUE)[1])^2 + (median - quantile(Z,na.rm=TRUE)[3])^2 + (UQR - quantile(Z,na.rm=TRUE)[5])^2
}


### (1/3) inpatient mengingitis costs
LQR = 115.06
median = 144.18
UQR = 331.49

time.start=proc.time()[[3]] #let's see how long this runs for
X_estimate = optim(c(2,97),minimise_this_gamma)
time.end=proc.time()[[3]]
time.end-time.start 

X_estimate

shape_estimate = X_estimate$par[1] #1.34, 1.33
scale_estimate = X_estimate$par[2] #173.49, 174,50

Z= rgamma(10000000, shape_estimate,scale=scale_estimate)
plot(density(Z))
mean(Z)
round(quantile(Z))

Z= rgamma(10000000, 1.34,scale=173.5)
plot(density(Z))
mean(Z)
round(quantile(Z))




### (2/3) inpatient pneumococcal costs
LQR = 117.1
median = 131.31
UQR = 318.61

time.start=proc.time()[[3]] #let's see how long this runs for
X_estimate = optim(c(1.34,174),minimise_this_gamma)
time.end=proc.time()[[3]]
time.end-time.start 

X_estimate

shape_estimate = X_estimate$par[1] #1.280074 
scale_estimate = X_estimate$par[2] #174.799947

Z= rgamma(10000000, shape_estimate,scale=scale_estimate)
plot(density(Z))
mean(Z)
round(quantile(Z))

Z= rgamma(10000000, 1.280074 ,scale=174.799947)
plot(density(Z))
mean(Z)
round(quantile(Z))

Z= rgamma(10000000, 1.28,scale=175)
plot(density(Z))
mean(Z)
round(quantile(Z))




### (3/3) home care costs
LQR = 0
median = 1.33
UQR = 2.96

time.start=proc.time()[[3]] #let's see how long this runs for
X_estimate = optim(c(1.34,1),minimise_this_gamma)
time.end=proc.time()[[3]]
time.end-time.start 

X_estimate

shape_estimate = X_estimate$par[1] #0.5709761 
scale_estimate = X_estimate$par[2] #3.9164354
#mean = 2.236867

Z= rgamma(10000000, shape_estimate,scale=scale_estimate)
plot(density(Z))
mean(Z)
round(quantile(Z),digits=2)


Z= rgamma(10000000, 0.57,scale=3.92)
plot(density(Z))
mean(Z)
round(quantile(Z),digits=2)



###(4/3) time sick
LQR = 9
median = 10
UQR = 12

Z= rgamma(10000000, 1.5,scale=5)
plot(density(Z))
mean(Z)
round(quantile(Z))

time.start=proc.time()[[3]] #let's see how long this runs for
X_estimate = optim(c(1.5,5),minimise_this_gamma)
time.end=proc.time()[[3]]
time.end-time.start 

X_estimate

shape_estimate = X_estimate$par[1] 
scale_estimate = X_estimate$par[2] 

Z= rgamma(10000000, shape_estimate,scale=scale_estimate)
plot(density(Z))
mean(Z)
round(quantile(Z),digits=2)





##previous iteration of function
minimise_this_gamma <- function(scale) {
  median = median
  LQR = LQR
  UQR = UQR
  
  Z= rgamma(10000000, 4, scale=scale)
  
  (LQR - quantile(Z)[2])^2 + (median - quantile(Z)[3])^2 + (UQR - quantile(Z)[4])^2  
}

LQR = 115.06
median = 144.18
UQR = 331.49


X_estimate = optimize(minimise_this_gamma, interval=c(0,200))
X = X_estimate$minimum

Z= rgamma(10000000, 4,scale=X)
plot(density(Z))
mean(Z)
quantile(Z)


plot(density(rgamma(10000, 5,scale = 0.75)))


