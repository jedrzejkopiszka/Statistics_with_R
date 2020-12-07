#Lab 4 - continuous random variable
#Ex1 
pbinom(3, 10, 0.1)
pnorm(3, 10*0.1, sqrt(10*0.1*0.9))  # approximation of binomial
pnorm(3, 10*0.1, sqrt(10*0.1*0.9)) - pnorm(0,10*0.1, sqrt(10*0.1*0.9))  #approximation of binomial

#Ex2
ex2_binom = rbinom(500, 20, 0.02)
ex2_poiss = rpois(500, 20*0.02)

par(mfrow=c(1,2))
discrete.hist(ex2_binom, freq = TRUE)
discrete.hist(ex2_poiss, freq = TRUE)

par(mfrow=c(1,2))
plot(ex2_binom, type="h", col=4)
plot(ex2_poiss, type="h", col=3)

par(mfrow=c(1,1))
curve(dnorm(x, 20*0.02, sqrt(20*0.02*0.98)), type="l", xlim = c(-1,2), ylim=c(0,1))
curve(dbinom(x, 20, 0.02), type="l", xlim = c(-1, 4), ylim=c(0,1))

#Ex3 exponential distribution using curve() and dexp()
curve(dexp(x, 3), type="l")

#Ex4 
1 - pexp(200, 0.01) # P(x>=200)
pexp(100, 0.01) #P(X<=100)
pexp(500, 0.01) #P(X<500)

#Ex5
ex5_lambda = 1/2.4
1 - pexp(3, ex5_lambda)
pexp(3, ex5_lambda) - pexp(2, ex5_lambda)

ex5_f = function (x) {
    dexp(x, ex5_lambda)
}
integrate(ex5_f, lower = 3, upper = Inf)
integrate(ex5_f, lower=2, upper = 3)

#Ex6

curve(dnorm(x, 0.13, 0.005), xlim=c(0.1, 0.2))
pnorm(0.14, 0.13, 0.005) - pnorm(0.12, 0.13, 0.005)

#Ex9
pnorm(50, 46.8, 1.75)
ex9_diff = 1.75*1.5
pnorm(46.8+ex9_diff, 46.8, 1.75) - pnorm(46.8-ex9_diff, 46.8, 1.75)


#Ex10
m = c()
for (i in 1:200){
  m = append(m ,mean(rnorm(30)))
}
curve(dnorm(x, 0, 1/sqrt(30)), xlim = c(-3*(1/sqrt(30)), 3*(1/sqrt(30))), col="red")
hist(m, freq = FALSE, add=TRUE)

#Ex11
ex11_m = c()
for (i in 1:200){
  m = append(m, mean(rpois(30, 4)))
}
breaks_ = seq(mean(m)-3*sd(m), mean(m)+3*sd(m), length = 100); breaks_
hist(m, freq=FALSE, xlim=c(mean(m)-3*sd(m), mean(m)+3*sd(m)), breaks = breaks_)
curve(dnorm(x, 4, 1/sqrt(30)), col="red", add=TRUE)
lines(density(m), add=TRUE, col="green")

#Ex12
pnorm(202, 200, 10/sqrt(25)) - pnorm(199, 200, 10/sqrt(25)) #average ressistance of 25
pnorm(5100, 25*200, 10/sqrt(25))  # total resistance of 25

#Ex13
pnorm(206, 202, 14/sqrt(64)) - pnorm(198, 202, 14/sqrt(64))

#Ex14
1 - pnorm(47, 100*0.5, 0.2/sqrt(100))

#Ex15
pnorm(20, 21, 2.9/sqrt(49))
pnorm(1050, 49*21, 2.9/sqrt(49)) - pnorm(980, 49*21, 2.9/sqrt(49))