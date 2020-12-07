#Lab 4
set.seed(633)
#Ex 1
pbinom(3, 10, 0.1)  # at most 3 defective
pnorm(3, 10*0.1, sqrt(10*0.1*0.9))  # approximation of binomial

pnorm(3, 10*0.1, sqrt(10*0.1*0.9)) - pnorm(0, 10*0.1, sqrt(10*0.1*0.9))

#Ex 2 
ex2_data_binom = rbinom(500, 20, 0.02)  
ex2_data_norm = rnorm(500, 20*0.02, sqrt(20*0.02*(1-0.02)))
ex2_data_pois = rpois(500, 20*0.02)

par(mfrow = c(3,1))
discrete.hist(ex2_data_binom, xlim=c(0,5))
discrete.hist(ex2_data_norm, xlim=c(0,5))
discrete.hist(ex2_data_pois, xlim=c(0,5))

par(mfrow = c(3,1))
x = seq(0, 20, by = 1)
plot(x, dbinom(x, 20, 0.02), type = "h", col = 2)
plot(x, dpois(x, 20*0.02), type="h", col = 4)
plot(x, dnorm(x, 20*0.02, sqrt(20*0.02*(1-0.02))), type="h", col = 3)

set.seed(633)
par(mfrow = c(2,1))
curve(dnorm(x, 20*0.02, sqrt(20*0.02*(1-0.02))), type="l", col="red", xlim=c(0,20), ylim=c(0,1))
hist(rbinom(500, 20, 0.02), xlim = c(0,n), add = TRUE, freq = FALSE)
lines(density(rbinom(500, 20, 0.02)), col="red", add=TRUE)

# Ex3 
par(mfrow = c(1,1))
curve(dexp(x, 3))

#Ex 4 
curve(dexp(x, 1/2.4), xlim = c(0,4))
1 - pexp(200, 0.01)
pexp(100, 0.01)
pexp(500, 0.01)

#Ex5
1 - pexp(3, 1/2.4)
pexp(3, 1/2.4) - pexp(2, 1/2.4)

lambda = 1/2.4

f = function(x){
  dexp(x, lambda)
}
integrate(f, 3, Inf)
integrate(f, 2, 3)

#Ex 6
pnorm(0.14, 0.13, 0.005)- pnorm(0.12, 0.13, 0.005)
curve(dnorm(x, 0.13, 0.005), xlim = c(0.1, 0.15))

f = function(x){
  dnorm(x, 0.13, 0.005)
}
integrate(f, 0.12, 0.14)

#Ex 7
pnorm(135, 120, 15) - pnorm(110, 120, 15)

#Ex 8
1 - pnorm(157, 200, sqrt(400))

#Ex9
pnorm(50, 46.8, 1.75)
1 - pnorm(48, 46.8, 1.75)
pnorm(46.8+1.5*1.75, 46.8, 1.75) - pnorm(46.8-1.5*1.75, 46.8, 1.75)

#Ex10
vec = c()
for (i in 1:200){
  vec = append(vec, mean(rnorm(30)))
}
vec
sd(vec)
par(mfrow = c(1,1))
hist(vec, freq = FALSE, xlim = c(mean(vec)-3*sd(vec), mean(vec) + 3*sd(vec)))
curve(dnorm(x, 0, 1/sqrt(30)), add=TRUE, col="red")
lines(density(vec), add=TRUE, col="green")

# Ex 11
size = 30
lambda = 4
vec_11 = c()
for (i in 1:200){
  vec_11 = append(vec_11, mean(rpois(size, lambda)))
}

par(mfrow=c(1,1))
hist(vec_11, freq = FALSE, xlim = c(mean(vec_11)-3*sd(vec_11), mean(vec_11)+3*sd(vec_11)))
curve(dnorm(x, lambda, 1/sqrt(size)), add=TRUE, col='red')
lines(density(vec_11), col="green")

#Ex 12
resistance = rnorm(25, 200, 10)
hist(resistance, freq = FALSE, xlim=c(200-3*10, 200+3*10))
curve(dnorm(x, 200, 10/sqrt(25)), add=TRUE)

pnorm(202, 200, 10/sqrt(25)) - pnorm(199, 200, 10/sqrt(25))

pnorm(5100, 25*200, 10/sqrt(25))

#Ex13
rnorm(64, 202, 14)
pnorm(206, 202, 14/sqrt(64)) - pnorm(198, 202, 14/sqrt(64))

#Ex14
rnorm(100, 0.5, 0.2)
1 - pnorm(47, 100*0.5, 0.2/sqrt(100))

#EX15
pnorm(20, 21, 2.9/sqrt(49))
pnorm(1050, 49*21.0, 2.9/sqrt(49)) - pnorm(980, 49*21.0, 2.9/sqrt(49))

#Ex16
variances = c()
for (i in 1:300){
  variances = append(variances, var(rnorm(40))*(40-1))
}

hist(variances,
      xlim=c(mean(variances)-3*sd(variances)), mean(variances)+3*sd(variances))
curve(dchisq(x, ))

