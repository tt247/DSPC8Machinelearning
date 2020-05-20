n <- 10^6
income <- 10^(rnorm(n, log10(45000),log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1, sample.kind = "Rounding")
N <- 250
X <- sample(income,N)
M <- median(X)
M

B <- 10^5
Ms <- replicate(B, {
  X <- sample(income, N)
  M <- median(X)
})


B <- 10^5
M_stars <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  M_star <- median(X_star)
})
quantile(Ms, c(0.05, 0.95))
quantile(M_stars, c(0.05, 0.95))


library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind = "Rounding") # if R 3.6 or later, set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
indexes

sum(indexes[[10]] ==3)

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
N <- 100

B <- 10000
ests <- replicate(B, {
  y <- rnorm(100, 0, 1)
  est <- quantile(y,0.75)
})

median(ests)
sd(ests)

y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
