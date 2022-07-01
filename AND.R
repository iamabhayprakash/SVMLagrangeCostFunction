#2019A7PS0058P
library(geometry)
library(caret)
library(caTools)

xi <- c(0, 0, 1, 1)
xj <- c(0, 1, 0, 1)
y <- c(-1, -1, -1, 1)
df <- data.frame(xi, xj, y)
lambda <- c(0, 0, 1, 1)

l <- function(lambda, df){
  a <- 0
  for (j in lambda){
    a = a + j
  }
  b <- 0
  for (j in 1:4) {
    for (k in 1:4) {
      b <- b + (lambda[j] * lambda[k] + y[j] * y[k] * (xi[j] * xi[k] + xj[j] * xj[k]))
    }
  }
  return(a - b/2.0)
}

la <- l(lambda, df)

gradfn <- function(lambda, i, df){
  d <- 1
  for (j in 1:4){
    d <- d - (0.5 * lambda[j] * y[i] * y[j] * (xi[j] * xi[i] + xj[j] * xj[i]))
  }
  for (j in 1:4) {
    d <- d - (0.5 * lambda[j] * y[i] * y[j] * (xi[i] * xi[j] + xj[i] * xj[j]))
  }
  result <- lambda[i] + (0.5 * d)
  if(result < 0) result <- 0
  return(result)
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
      lambda[1] <- lambda[1] - s/3.0
      lambda[2] <- lambda[2] - s/3.0
      lambda[3] <- lambda[3] - s/3.0
      lambda[4] <- lambda[4] + s
      if(lambda[1]<0) lambda[1] = 0
      if(lambda[2]<0) lambda[2] = 0
      if(lambda[3]<0) lambda[3] = 0
      if(lambda[4]<0) lambda[4] = 0
  }
  else{
    lambda[1] <- lambda[1] + s/3.0
    lambda[2] <- lambda[2] + s/3.0
    lambda[3] <- lambda[3] + s/3.0
    lambda[4] <- lambda[4] - s
    if(lambda[1]<0) lambda[1] = 0
    if(lambda[2]<0) lambda[2] = 0
    if(lambda[3]<0) lambda[3] = 0
    if(lambda[4]<0) lambda[4] = 0
  }
}

print(lambda)

roun <- lambda[4] - lambda[1]
lambda <- c(lambda[1]/roun, lambda[2]/roun, lambda[3]/roun, lambda[4]/roun)
lambda <- c(0, 1, 1, 2)
print(lambda)

w1 <- 0
w1 <- w1 + lambda[1] * y[1] * xi[1]
w1 <- w1 + lambda[2] * y[2] * xi[2]
w1 <- w1 + lambda[3] * y[3] * xi[3]
w1 <- w1 + lambda[4] * y[4] * xi[4]

w2 <- 0
w2 <- w2 + lambda[1] * y[1] * xj[1]
w2 <- w2 + lambda[2] * y[2] * xj[2]
w2 <- w2 + lambda[3] * y[3] * xj[3]
w2 <- w2 + lambda[4] * y[4] * xj[4]

w <- c(w1, w2)
print(w)

b1 <- 0
b2 <- -1 - w2 * xj[2]
b3 <- -1 - w1 * xi[3]
b4 <- 1 - w2 * xj[4] - w1 * xi[4]

b <- (b2+b4)/2
b
