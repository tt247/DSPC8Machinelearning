set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1

B <- 100

rmse <- replicate(B,{
  y <- dat$y 
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
  train_set <- dat %>% slice(-test_index) 
  test_set <- dat %>% slice(test_index) 
  fit <- lm(y ~ x, data = train_set) 
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x 
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rmse)
sd(rmse)

#Q2


set.seed(1, sample.kind="Rounding")
n <- c(100,500,1000,5000,10000)
rmseengine <- function(n){

Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
data.frame() %>% setNames(c("x", "y"))

rmse<-(replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
}))

result <- c(sd(rmse),mean(rmse))
result
return(result)
}

a<-sapply(n,rmseengine)
a

#I solved it with above, model answer is as follows

set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

#Q4

set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

B <- 100
set.seed(1, sample.kind="Rounding") 
rmse <- replicate(B,{
  y <- dat$y 
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
  train_set <- dat %>% slice(-test_index) 
  test_set <- dat %>% slice(test_index) 
  fit <- lm(y ~ x, data = train_set) 
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x 
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rmse)
sd(rmse)

#Q6

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

y <- dat$y 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
train_set <- dat %>% slice(-test_index) 
test_set <- dat %>% slice(test_index) 
fit1 <- lm(y ~ x_1, data = train_set) 
y_hat1 <- fit$coef[1] + fit$coef[2]*test_set$x_1 
rmse1<-sqrt(mean((y_hat1 - test_set$y)^2))
fit2 <- lm(y ~ x_2, data = train_set) 
y_hat2 <- fit$coef[1] + fit$coef[2]*test_set$x_2
rmse2 <- sqrt(mean((y_hat2 - test_set$y)^2))
fit3 <- lm(y ~ x_1 + x_2, data = train_set) 
y_hat3 <- fit$coef[1] + fit$coef[2]*(test_set$x_1+test_set$x_2)
rmse3 <- sqrt(mean((y_hat3 - test_set$y)^2))
rmse1
rmse2
rmse3

#correct answer

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

#Q7

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
