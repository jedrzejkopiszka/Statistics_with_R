#Lab 1 - introduction to R
#Ex 1 - trigonometric functions
sin(2*pi)
cos(3/4)
tan(pi)
log(15)
log(exp(4))
log(1/7, base = 7)

#Ex2 - vector 1:10
c(1:10)
sum(c(1:10))

#Ex3 - vector with even numbers from 2 to 20
ex3 = c(seq(2,20,2))
ex3_2 = c()
append_vec = function(){
  for (i in 1:length(ex3)){
    ex3_2 = append(ex3_2, ex3[i]^2)
  }
  ex3_2
}
ex3_2 = append_vec() #x^2

rev(ex3_2)

#Ex4 - generating vector
seq(5, 10, length.out = 13) 

#Ex5 - 5 times the same vector 
ex5 = c(1,2)
rep(ex5, 5)

#Ex6 working with vectors
ex6 = c(1,3,6,2,7,4)
min(ex6)
which.min(ex6)  # index of the smallest value in vector
which(ex6<4)  #indexes of values smaller than 4
sum(ex6^2)  # sum of squares
ex6+4
ex6[-4]  #4th element excluded
ex6[which(ex6>4)]  #vector with elements greater than 4

#Ex7 - intro to matrices
ex7_r1 = c(2,3,0)
ex7_r2 = c(3,1,5)
ex7_r3 = c(0,4,4)

ex7 = rbind(ex7_r1, ex7_r2, ex7_r3); ex7

det(ex7)  # determinant of matrix
t(ex7)  # transpose matrix
solve(ex7)  # inverse of a matrix
ex7^2  
diag(ex7)  # main diagonal of matrix
ex7*diag(ex7)  # matrix multiplication


#Ex8 working with matrices
ex8 = rbind(c(1,2,4), c(3,1,5), c(0,4,4))  #matrix 3x3
det(ex8)  #determinant
t(ex8)  #transpose
solve(ex8)  #inverse
ex8*solve(ex8)  
diag(ex8)  #diagonal of ex8
ex8[2,]  #2nd row
ex8[,2]  #2nd column
