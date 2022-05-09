minimise_this_lognormal <- function(sd) {
  mean = mean
  mean_sq = mean^2
  sd_sq = sd^2
  LQR = LQR
  UQR = UQR
  
  a = log(mean_sq/sqrt(mean_sq+sd_sq))
  b = log(1+sd_sq/mean_sq)
  
  Z= rlnorm(10000000, meanlog = a,sdlog = b)
  
  LQR_estimate <- as.numeric(quantile(Z)[2])
  UQR_estimate <- as.numeric(quantile(Z)[4])
  (LQR - LQR_estimate)^2 + (UQR - UQR_estimate)^2
}

# pneumoccocal meningitis
mean = 144.18
UQR = 331.49
LQR = 115.06

sd = optimize(minimise_this_lognormal, interval=c(0, 200))
sd = sd$minimum

mean_sq = mean^2
sd_sq = sd^2

a = log(mean_sq/sqrt(mean_sq+sd_sq))
b = log(1+sd_sq/mean_sq)

Z= rlnorm(10000000, meanlog = a,sdlog = b)
plot(density(Z),xlim=c(0,600))
mean(Z)
quantile(Z)
