
#COSTS
#gamma when provided with mean and sd
mean = 0.95
sd = 0.75

a = (mean/sd)^2 #1.6
b = (sd^2)/mean #0.59

Z= rgamma(10000000, a, scale=b)
plot(density(Z))
mean(Z)
sd(Z)



Z = rbeta(100000,25,75)
plot(density(Z))
mean(Z)
sd(Z)


