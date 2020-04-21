#Compare accuracy of previous height predictions by gender
table(predicted = y_hat, actual = test_set$sex)

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

#Prevalance is the problem

prev <- mean(y == "Male")
prev

#Metrics other than accuracy to help evaluate algorithm

confusionMatrix(data = y_hat, reference = test_set$sex)

#balanced accuracy and F1 score

#rebuilding algo to maximise F-score

cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height>x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels (test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)
