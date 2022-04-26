############################# boostrap simulation

x <- rexp(1000,1)
y <- rpois(1000,2)


set.seed(24)
bootstrp = function(x){

  mean.x = c()
  sd.x = c()
  
  for(b in 1:10000){
    xi = sample(x, 100, replace = TRUE)
    mean.x[b] = mean(xi)
    sd.x[b] = sd(xi)
  }
  
  par(mfrow = c(1, 2), pch = 19)
  hist(mean.x,breaks = 50)
  hist(sd.x,breaks = 50)
}


# histogram for exp distribution
bootstrp(x)

# histogram for pois distribution
bootstrp(y)




############################# for exp(1) distribution, we evalute the mean
x <- rexp(1000,1)
mean.x = c()

for(b in 1:10000){
  xi = sample(x, 100, replace = TRUE)
  mean.x[b] = mean(xi)
}



# application1: Bias & corrected mean
bs_mean = mean(mean.x);bsmean
sample_mean = mean(x);sample_mean
bais = bsmean-sample_mean;bais # the ture mean is 1
corrected_mean = mean(x) - bais;corrected_mean

# application2: CI
bs_sd = sd(mean.x);bs_sd
quantile(mean.x,c(0.025,0.975)) # the ture mean 1 is included in the CI

# application3: MSE
MSE = bais^2 + bs_var;MSE


