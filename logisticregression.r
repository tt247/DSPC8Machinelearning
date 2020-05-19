# Logistic regression

glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data =., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)

# Case study

data("mnist_27")

fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat>0.5, 7,2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color = "black")
