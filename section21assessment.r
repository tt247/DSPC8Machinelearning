dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1

head(dat)
sum(dat$sex == "Female" & dat$type == "inclass")/sum(dat$type == "inclass")
sum(dat$sex == "Female" & dat$type == "online")/sum(dat$type == "online")

#Q2 

y <- dat$sex
y_hat <- ifelse(dat$type == "online", "Male", "Female") %>%
  factor(levels = levels(y))

mean(y == y_hat)

#Q3
table(y_hat, y)

#Q4,5,6,7

d <- confusionMatrix(data = y_hat, reference = y)
d

#Q7
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")

test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#Q8

head(iris)

min(iris$Sepal.Length)
max(iris$Sepal.Length)

cutoff <- seq(4.9,8,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat==train$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Sepal.Width)
max(iris$Sepal.Width)

cutoff <- seq(2,4,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat==train$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Petal.Length)
max(iris$Petal.Length)

cutoff <- seq(3,7,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat==train$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Petal.Width)
max(iris$Petal.Width)

cutoff <- seq(1,2.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat==train$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Q9

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

#Q10

min(iris$Sepal.Length)
max(iris$Sepal.Length)

cutoff <- seq(4.9,8,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat==test$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Sepal.Width)
max(iris$Sepal.Width)

cutoff <- seq(2,4,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat==test$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Petal.Length)
max(iris$Petal.Length)

cutoff <- seq(3,7,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat==test$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Petal.Width)
max(iris$Petal.Width)

cutoff <- seq(1,2.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat==test$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Q11
plot(iris,pch=21,bg=iris$Species)

cutoff <- seq(3,7,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat==train$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(iris$Petal.Width)
max(iris$Petal.Width)

cutoff <- seq(1,2.5,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat==train$Species)
})

max(accuracy)

altbest_cutoff <- cutoff[which.max(accuracy)]
altbest_cutoff

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", ifelse(test$Petal.Width > altbest_cutoff, "virginica", "versicolor")) %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)
