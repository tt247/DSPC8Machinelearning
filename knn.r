library(caret)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

knn_fit_1 <- knn3(y~.,data=mnist_27$train, k=1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]

knn_fit_401 <- knn3(y~.,data=mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y~.,data=mnist_27$train, k=k)
  y_hat<-predict(fit,mnist_27$train,type = "class")
  train_error <- confusionMatrix(data=y_hat, reference=mnist_27$train$y)$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
  list(train = train_error, test = test_error)
})

#Comprehension check

set.seed(1,sample.kind="Rounding")



data("heights")
y <- heights$sex
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)


ks <- seq(1, 101, 3)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, train_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train_set$sex)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$sex)
  test_error <- cm_test$overall["Accuracy"]
  f <- F_meas(data = y_hat, reference = factor(test_set$sex))
  tibble(train = train_error, test = f)
})

ks[which.max(accuracy$test)]
max(accuracy$test)

#Correct answer

library(dslabs)
library(tidyverse)
library(caret)
data("heights")

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

#Q2 

str(tissue_gene_expression)
library(dslabs)
data("tissue_gene_expression")
tgedf<-data.frame(x=tissue_gene_expression$x,  y= tissue_gene_expression$y) 
set.seed(1,sample.kind="Rounding")
test_index <- createDataPartition(tgedf$y, times = 1, p = 0.5, list = FALSE)
test_set <- tgedf[test_index, ]
train_set <- tgedf[-test_index, ]  

ks <- seq(1, 3, 5, 7, 9, 11)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = train_set, k = k)
  y_hat <- predict(fit, train_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train_set$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$y)
  test_error <- cm_test$overall["Accuracy"]
  tibble(train = train_error, test = test_error)
})

accuracy

#correct answer

set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})
