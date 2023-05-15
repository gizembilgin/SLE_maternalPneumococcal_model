
minimise_this_beta <- function(X) {
  mean = mean
  
  a = mean * X
  b = (1-mean) * X
  
  Z= rbeta(10000000, a, b)
  #LB_estimate <- as.numeric(quantile(Z)[1])
  #UB_estimate <- as.numeric(quantile(Z)[5])
  
  LB_estimate <- as.numeric(quantile(Z,.025))
  UB_estimate <- as.numeric(quantile(Z,.975))
  
  (LB - LB_estimate)^2 + (UB - UB_estimate)^2 #squared residuals
  
  #(dbeta(LB,a,b))^2+(1-dbeta(UB,a,b))^2
}

#outpatient care, NB: this is with 95% confidence interval
mean = 0.857; UB = 90.5; LB = 79.8; a=84.11; b=14.04 #84 and 14 are good enough

mean=0.652; UB = 0.714; LB = 0.475 # access to inpatient care
#a = 55.51489; b = 29.89263

#GBD with 95%
mean = 0.133; UB = 0.19; LB = 0.088 #a=13.29; b=86.62
mean = 0.051; UB = 0.074; LB = 0.032
mean = 0.542; UB = 0.702; LB = 0.374


X_estimate = optimize(minimise_this_beta, interval=c(0,100))
X = X_estimate$minimum

a = mean * X
b = (1-mean) * X
a;b

#a=13.29; b=86.62


Z= rbeta(10000000, a,b)
plot(density(Z))
mean(Z)
quantile(Z); quantile(Z,c(.025,.975))



