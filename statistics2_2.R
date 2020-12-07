# Lab 3 - Discrete Random Variables
#Ex1
ex1_t = rbind(c(-5, -2, 0, 1, 3, 8), c(0.1, 0.2, 0.1, 0.2, 0.3, 0.1))
ex1_t

#Expected value
ex = function(data){
  summary_ = 0
  for (i in 1:length(data[1,])){
    summary_ = summary_ + data[1,i]*data[2,i]
  }
  summary_
}
ex(ex1_t) 

# standard deviation
std = function(data, mean){
  suma = 0
  for (i in 1:length(data[1,])){
    suma = suma + (data[1,i]^2*data[2,i] - mean^2)
  }
  sqrt(suma)
}
std(ex1_t, ex(ex1_t))

#Ex 2 - hard!

#Ex3 - binomial distribution
ex3_n = 2
ex3_defective = 10
ex3_p = ex3_defective/120
ex3_n*ex3_p  # EX = n*p  for binomial
sqrt(ex3_n*ex3_p*(1-ex3_p))  # STD = sqrt(n*p*(1-p)) for binomial

#Ex4
ex4_p = 0.3
ex4_n = 5
dbinom(3, ex4_n, ex4_p)
1 - pbinom(2.99, ex4_n, ex4_p)
pbinom(2.99, ex4_n, ex4_p)

#Ex5
dbinom(8, 8, 0.9)  # P(B==8)
dbinom(7,8,0.9)  # P(B==7)
1 - pbinom(5, 8, 0.9)  # P(B>5)
0.9*8  # EX
sqrt(0.9*8*(1-0.9))  # SD

#EX6
pbinom(3, 10, 0.1)  
ppois(3, 10*0.1)  # approximation of binomial by poisson

#Ex7
1 - ppois(2.99, 100*0.05)  # approximation by poisson
1 - pbinom(2.99, 100, 0.05)  # exact value

#Ex8
ex8 = function(){
  suma = 0
  for (i in 0:3){
    prob = dbinom(i, 3, 0.2)
    suma = suma + ((3*i*i + i)*prob)
  }
  suma
}

ex8()




