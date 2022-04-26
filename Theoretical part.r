############################# boostrapping simulation
set.seed(1)

exp1000 <- rexp(1000,1)
pois1000 <- rpois(1000,2)



bootstrp = function(x){
  
  mean.x = c()
  sd.x = c()
  
  for(b in 1:10000){
    xi = sample(x, length(x), replace = TRUE)
    mean.x[b] = mean(xi)
    sd.x[b] = sd(xi)
  }
  
  par(mfrow = c(1, 2), pch = 19)
  hist(mean.x,breaks = 20)
  hist(sd.x,breaks = 20)
}


# histogram for exp distribution
bootstrp(exp1000)

# histogram for pois distribution
bootstrp(pois1000)




############################# for exp(1) distribution, we evaluate the mean
x <- rexp(1000,1)
mean.x = c()

for(b in 1:10000){
  xi = sample(x, length(x), replace = TRUE)
  mean.x[b] = mean(xi)
}



# application1: Bias & corrected mean
bs_mean = mean(mean.x);bs_mean
sample_mean = mean(x);sample_mean

bais           = bs_mean - sample_mean;bais # the true mean is 1
corrected_mean = sample_mean - bais;corrected_mean

# application2: CI
bs_sd = sd(mean.x);bs_sd
quantile(mean.x,c(0.025,0.975)) # the true mean 1 is included in the CI

# application3: MSE
MSE = bais^2 + bs_sd^2;MSE


