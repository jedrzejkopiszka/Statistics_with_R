#145304 kopiszka jedrzej
data = read.csv("descriptive.csv", sep=";"); data

#Ex1
ex1_data = c(na.omit(data$mouse))
# a) Median and quantiles 
quantile(ex1_data)
#Median equals to 5.485, quartiles: Oth: 0.18, 1st=3.95, 3rd=7.4225, 4th=12.39
#Median is the value in the middle of the sorted list of numbers, quartiles measure central tendency
#4th quartile shows the maximum value of 12.39, 0th quartile i 0.18 (minimum)

#b)probability hist 
par(mfrow=c(1,1))
hist(ex1_data, freq = FALSE,
     main="Data distribution of tumor in mice", 
     xlab = "tumor inside mouse")

#Ex2
ex2_table = rbind(c(4,3,2,1), c(7,16,25,12)); ex2_table #Construct a table

#a) probability distribution
ex2_distr = function(data){
  suma = sum(data[2,])
  new_vec = c()
  for ( i in 1:length(data[1,])){
    new_vec = append(new_vec, data[2,i]/suma)
  }
  rbind(data[1,], new_vec)
}
ex2_distr(ex2_table)  #probability distribution table
#years: 4.0000000 3.0000000 2.0000000  1.0
#probability: 0.1166667 0.2666667 0.4166667  0.2

#Expectation
ex2_ex = function(data){
  suma = 0
  for (i in 1:length(data[1,])){
    suma = suma + data[1,i]*data[2,i] #Xi*Pi
  }
  suma
}
ex2_distr_tab = ex2_distr(ex2_table)
ex2_ex(ex2_distr_tab)
#Expected length of work is 2.3 years

#E(X^2)
ex2_expected_squared = function(data){ 
  suma = 0
  for (i in 1:length(data[1,])){
    suma = suma + data[1,i]*data[1,i]*data[2,i]
  }
  suma
}

a = ex2_expected_squared(ex2_distr_tab)  # E(X^2)
a - (ex2_ex(ex2_distr_tab))^2 # E(X^2) - EX^2
#Standard deviation is 0.84

#Probability that a worker works more than x years
ex2_prob = function(year, data){
  suma = 0
  for ( i in 1:length(data[1,])){
    if(data[1,i]>year){
      suma = suma + data[2,i]
    }
  }
  suma
}

ex2_prob(2, ex2_distr_tab)   
# Probability that a worker works for the company longer than 2 years is 0.38

#Ex3
set.seed(145304)
ex3_binom07 = rbinom(500, 30, 0.07)
ex3_binom47 = rbinom(500, 30, 0.47)

par(mfrow=c(1,2))
hist(ex3_binom07, 
     freq = FALSE,
     main = "Binomial distribution for n=30 and p=0.07 and its approximation")
curve(dnorm(x, 30*0.07, sqrt(30*0.07*(1-0.07))),
      add=TRUE, 
      col="red")

hist(ex3_binom47, 
     freq = FALSE,
     main = "Binomial distribution for n=30 and p=0.47 and its approximation")
curve(dnorm(x, 30*0.47, sqrt(30*0.47*(1-0.47))),
      add=TRUE, 
      col="red")

#Ex4
ex4_mean = 260
ex4_std = 150
ex4_n = 10000
#a) distribution
curve(dnorm(x, ex4_n*ex4_mean, ex4_std/sqrt(ex4_n)))

#b) Probability of total yearly claim does not exceed 2.5 million
pnorm(2500000, 10000*260, 150/sqrt(ex4_n)) # Returns 0, but that is not the true result


#Ex5
ex5_n = 120
ex5_beside = 24
ex5_p = ex5_beside/ex5_n; ex5_p #p_hat = 0.2
ex5_conf = 0.98
binom.test(ex5_beside, ex5_n, conf.level = ex5_conf)$conf.int # Confidence interval is 0.1221361 0.2984894
# Interpretation: With confidence 98% proportion falls between 0.12 and 0.3. This means 12% to 30% percent of the
#work is done outside the workplace.

#Punishment: Proportion is very likely to exceed 0.15 as our proportion with 98% of confidence is between 0.12 and 0.3,
# this means that with high probability more than 15% of work is done outside workplace, though punishment should be based on
# evidence not high probability of committing a crime. To punish, one must base on population not sample.
