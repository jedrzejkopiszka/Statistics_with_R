# Lab 6,7 - Estimation
data = read.csv("data_est.csv", sep=";")

#Ex1 
ex1 = c(na.omit(data$diamonds))
mean(ex1)
var(ex1)
sd(ex1)
# Confidence interval for mean estimation, sd unknown
ex1_conf = function(conf, data){
  conf = 1- conf
  left = qt(1-conf/2, length(data))*sd(data)/sqrt(length(data))
  right = mean(data) + left
  left = mean(data) - left
  
  c(left, right)
}
ex1_conf(0.95, ex1)
t.test(ex1, conf.level = 0.95) #95% confidence interval for ex1 data

#Ex2 
ex2 = c(na.omit(data$milk)); ex2
mean(ex2)
var(ex2)
sd(ex2)
t.test(ex2, conf.level = 0.95)$conf.int  #take only confidence interval
sigma.test(ex2, conf.level = 0.95)  # variance
sqrt(sigma.test(ex2, conf.level = 0.95)$conf.int) #std

#Ex3
ex3 = c(na.omit(data$cigarettes))
ex3_conf = function(conf, data){
  conf = 1 - conf
  left = qnorm(1-conf/2)*0.7/sqrt(length(data))
  right = mean(data) + left 
  left = mean(data) - left
    
  c(left, right)
}

ex3_conf(0.95, ex3)

# How many datapoints minimum to get 0.95
ex3_size = function(conf, data){
  conf = 1 - conf
  n = (qnorm(1-conf/2)*0.7/0.15)^2
  n
}
ex3_size(0.95, ex3)

#Ex4
ex4 = c(na.omit(data$signal))
z.test(ex4, sigma.x = 3, conf.level = 0.95)$conf.int

#Ex5
ex4_conf = function(conf){
  conf = 1- conf
  left = qnorm(1-conf/2)*2.2/sqrt(1200)
  right = 4.7 + left
  left = 4.7 - left
    
  c(left, right)
}
ex4_conf(0.95)

#Ex7
ex7_f = function(conf, variance, error){
  conf = 1-conf
  n = (qnorm(1-conf/2)*sqrt(variance)/(0.5*error))^2
  n 
}
ceiling(ex7_f(0.95, 25, 1))

#Ex8

ex8_f = function(conf, std, error){
  conf = 1 - conf
  n = (qnorm(1-conf/2)*std/(0.5*error))^2
  ceiling(n)
}

ex8_f(0.99, 0.3, 0.2)

#ex9
ex9_f = function(conf, p, n){
  conf = 1- conf
  left = qnorm(1-conf/2)*sqrt(p*(1-p)/n)
  right = p + left
  left = p - left
  
  c(left, right)
}

ex9_f(0.95, 0.03, 100)
binom.test(3, 100, conf.level = 0.95)$conf.int
prop.test(3, 100, conf.level=0.95)$conf.int

#Ex10
binom.test(4, 100, conf.level = 0.95)$conf.int

#Ex11
binom.test(24, 120, conf.level = 0.9)$conf.int

#Ex12
binom.test(122, 1000, conf.level = 0.9)$conf.int
