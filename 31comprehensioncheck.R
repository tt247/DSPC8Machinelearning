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

B <- 100

rmse <- replicate(B,{
  y <- dat$y 
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
  train_set <- dat %>% slice(-test_index) 
  test_set <- dat %>% slice(test_index) 
  fit <- lm(y ~ x, data = train_set) 
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x 
  sqrt(mean((y_hat - test_set$y)^2))
mean(rmse)
  })

mean(rmse)
sd(rmse)
}
sapply(n,rmseengine)

