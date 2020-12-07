#Lab2 Descriptive statistics
#Ex1
ex1_1 = rbinom(20, 10, 0.3)
ex1_2 = rbinom(20, 10, 0.3)
mean(ex1_1)
quantile(ex1_2)

# Variability measures
ex1_1[length(ex1_1)] - ex1_1[1] # Range
quantile(ex1_1)[4] - quantile(ex1_2)[1]  #interquartile range Q3 - Q1
sd(ex1_1)/mean(ex1_1)  # variability index
var(ex1_1)  #variance
sd(ex1_1)  # standard deviation

#quantiles
quantile(ex1_1, c(0.1, 0.3, 0.6, 0.9))

c(ex1_1, ex1_2) #merge two vectors
sort(c(ex1_1, ex1_2))

#Ex2 
ex2_1 = ex1_1[1:8]
ex2_2 = ex1_2[1:8]
ex2_3 = letters[1:8]

ex2_m1 = rbind(ex2_1, ex2_2, ex2_3)
ex2_d1 = data.frame(ex2_m1)

#Ex4
ex4_data = read.csv("flights.csv", sep=';')
length(ex4_data)  #gives number of columns

par(mfrow = c(2,3))
for (i in 1:length(ex4_data)){
  data = ex4_data[,i]
  hist(data,
       freq = FALSE, 
       breaks = seq(min(data), max(data), length = 6),
       xlim = c(min(data), max(data)),
       main = paste0("Year", i))
}

par(mfrow=c(2,3))
for (i in 1:length(ex4_data)){
  data = ex4_data[,i]
  boxplot(data,
          main=paste0("Variability of ", colnames(ex4_data)[i]),
          ylab="passangers",
          ylim=c(min(data), max(data)))
}

#Ex5
ex5_data = read.csv("strawberries.csv", sep = ";")
var(na.omit(ex5_data$crop.2010))


#Ex6 - important line graphs, dec=','
ex6_data = read.csv("notes.csv", sep=";", dec = ",")

par(mfrow=c(2,2))
for (i in 1:length(ex6_data)){
  data = na.omit(ex6_data[,i])
  #line chart 
  plot(data,
       type="o",
       ylim = c(min(data)-1, max(data)+1),
       main = paste0("Line chart of ", colnames(ex6_data)[i]),
       col="red")  
}

#Discrete histogram - frequency 
for (i in 1:length(ex6_data)){
  data = na.omit(ex6_data[,i])
  discrete.hist(data,
                freq = TRUE)
}

#Box plots
for (i in 1:length(ex6_data)){
  data = na.omit(ex6_data[,i])
  boxplot(data,
          ylim = c(min(data)-1, max(data)+1),
          main = paste("Box plot for", colnames(ex6_data)[i]))
}


# Pie charts
for(i in 1:length(ex6_data)){
  data = na.omit(ex6_data[,i])
  pie(table(data))
}

#Frequency tables? how to do them?