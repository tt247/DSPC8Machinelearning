#make sure you have loaded libraries in library script

y <- heights$sex
x <- heights$height

set.seed(2, sample.kind="Rounding")

#Creating test data and learning data

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- heights[-test_index,]
test_set <- heights[test_index,]

#Guessing the outcome

y_hat <- sample(c("Male", "Female"),
                length(test_index), replace = TRUE)

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

#Exploratory data analysis

heights %>% group_by(sex) %>%
  summarize(mean(height), sd(height))

#Simple approach based on data

y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

mean(y == y_hat)

#Trying different height cutoffs

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat==train_set$sex)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Evaluating now on test data 

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


