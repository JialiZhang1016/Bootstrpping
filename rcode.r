
x1 <- rpois(1000,2)
x2 <- runif(1000,5,500)
x3 <- rnorm(1000,0,50)
x4 <- rchisq(1000,5)



set.seed(123)
bootstrp = function(x){
  sd.x = c()
  mean.x = c()
  median.x = c()

  
  for(b in 1:10000){
    xi = sample(x, 100, replace = TRUE)
    sd.x[b] = sd(xi)
    mean.x[b] = mean(xi)
    median.x[b] = median(xi)
    
  }
  
  par(mfrow = c(2, 2), pch = 19)
  hist(mean.x,breaks = 50)
  hist(sd.x,breaks = 50)
  hist(median.x,breaks = 50)
}


bootstrp(x1)
bootstrp(x2)
bootstrp(x3)
bootstrp(x4)

setwd('C:\\Users\\Jiali Zhang\\Documents\\2022 Spring\\Stat5346 regression\\project')
data <- read.csv('student-por.csv')

summary(data)
data$school <- factor(data$school)
summary(data)

GPG1 <- subset(data,school = 'GP')[,c(1,2)]
GPG2 <- subset(data,school = 'GP')[,c(1,3)]
GPG3 <- subset(data,school = 'GP')[,c(1,4)]
MSG1 <- subset(data,school = 'MS')[,c(1,2)]
MSG2 <- subset(data,school = 'MS')[,c(1,3)]
MSG3 <- subset(data,school = 'MS')[,c(1,4)]

