#2019A7PS0058P
library(geometry)
library(caret)
library(caTools)

xi <- c(0, 0, 1, 1)
xj <- c(0, 1, 0, 1)
y <- c(-1, 1, 1, -1)

t1 <- c(1, 1, 1, 1)
t2 <- c(0, 0, 1, 1)
t3 <- c(0, 1, 0, 1)
t4 <- c(0, 0, 1,414, 1,414)
t5 <- c(0, 1.414, 0, 1.414)
t6 <- c(0, 0, 0, 1.414)

df <- data.frame(xi, xj, y)
lambda <- c(0, 1, 0, 1)

gradfn <- function(lambda, i, df){
  d <- 1
  for (j in 1:4){
    d <- d - (0.5 * lambda[j] * y[i] * y[j] * (t1[j] * t1[i] + t2[i]*t2[j] + t3[i] * t3[j] + t4[i] * t4[j] + t5[i] * t5[j] + t6[i] * t6[j]))
  }
  for (j in 1:4) {
    d <- d - (0.5 * lambda[j] * y[i] * y[j] * (t1[j] * t1[i] + t2[i]*t2[j] + t3[i] * t3[j] + t4[i] * t4[j] + t5[i] * t5[j] + t6[i] * t6[j]))
  }
  return(lambda[i] + (0.00005 * d))
}

for (i in 1:20) {
  a <- gradfn(lambda, 1, df)
  b <- gradfn(lambda, 2, df)
  c <- gradfn(lambda, 3, df)
  d <- gradfn(lambda, 4, df)
  lambda[1] <- a
  lambda[2] <- b
  lambda[3] <- c
  lambda[4] <- d
  
  s <- 0
  s <- s + a * y[1]
  s <- s + b * y[2]
  s <- s + c * y[3]
  s <- s + d * y[4]
  s <- s/2.0
  if(s > 0){
    lambda[1] <- lambda[1] - s/2.0
    lambda[2] <- lambda[2] + s/2.0
    lambda[3] <- lambda[3] + s/2.0
    lambda[4] <- lambda[4] - s/2.0
    if(lambda[1]<0) lambda[1] = 0
    if(lambda[2]<0) lambda[2] = 0
    if(lambda[3]<0) lambda[3] = 0
    if(lambda[4]<0) lambda[4] = 0
  }
  else{
    lambda[1] <- lambda[1] + s/2.0
    lambda[2] <- lambda[2] - s/2.0
    lambda[3] <- lambda[3] - s/2.0
    lambda[4] <- lambda[4] + s/2.0
    if(lambda[1]<0) lambda[1] = 0
    if(lambda[2]<0) lambda[2] = 0
    if(lambda[3]<0) lambda[3] = 0
    if(lambda[4]<0) lambda[4] = 0
  }
}
lambda <- c(1, 0, 1, 0)
print(lambda)

w1 <- 0
w1 <- w1 + lambda[1] * y[1] * t1[1]
w1 <- w1 + lambda[2] * y[2] * t1[2]
w1 <- w1 + lambda[3] * y[3] * t1[3]
w1 <- w1 + lambda[4] * y[4] * t1[4]

w2 <- 0
w2 <- w2 + lambda[1] * y[1] * t2[1]
w2 <- w2 + lambda[2] * y[2] * t2[2]
w2 <- w2 + lambda[3] * y[3] * t2[3]
w2 <- w2 + lambda[4] * y[4] * t2[4]

w3 <- 0
w3 <- w3 + lambda[1] * y[1] * t3[1]
w3 <- w3 + lambda[2] * y[2] * t3[2]
w3 <- w3 + lambda[3] * y[3] * t3[3]
w3 <- w3 + lambda[4] * y[4] * t3[4]

w4 <- 0
w4 <- w4 + lambda[1] * y[1] * t4[1]
w4 <- w4 + lambda[2] * y[2] * t4[2]
w4 <- w4 + lambda[3] * y[3] * t4[3]
w4 <- w4 + lambda[4] * y[4] * t4[4]

w5 <- 0
w5 <- w5 + lambda[1] * y[1] * t5[1]
w5 <- w5 + lambda[2] * y[2] * t5[2]
w5 <- w5 + lambda[3] * y[3] * t5[3]
w5 <- w5 + lambda[4] * y[4] * t5[4]

w6 <- 0
w6 <- w6 + lambda[1] * y[1] * t6[1]
w6 <- w6 + lambda[2] * y[2] * t6[2]
w6 <- w6 + lambda[3] * y[3] * t6[3]
w6 <- w6 + lambda[4] * y[4] * t6[4]

w <- c(w1, w2, w3, w4, w5, w6)
print(w)

w

b1 <- -1 - t1[1]*w[1] + t2[1]*w[1] + t3[1]*w[1] + t4[1]*w[1] + t5[1]*w[1] + t6[1]*w[1]
b3 <- 1 - t1[3]*w[3] + t2[3]*w[3] + t3[3]*w[3] + t4[3]*w[3] + t5[3]*w[3] + t6[3]*w[3]
b1
b3

b <- (b1+b3)/2
b
