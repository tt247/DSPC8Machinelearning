#Q1
(0.02*0.85)*(0.02/0.85)
100*0.02
2*0.85
98*0.1
9.8+1.7
1.7/11.5

#Q2

set.seed(1, sample.kind="Rounding")
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#first line is my solution completely unnecessary as it turns out
(mean(test[disease==0])*length(test[disease==0])+mean(test[disease==1])*length(test[disease==1]))/(length(test[disease==1])+length(test[disease==0]))
mean(test)

#Q3

#again my answer is apparently right but unnecessarily complex
truenegative<- length(test[disease==0])-sum(test[disease==0])
falsenegative<- length(test[disease==1])-sum(test[disease==1])
falsenegative/(truenegative+falsenegative)

#simple method
mean(disease[test==0])

#Q4

mean(disease[test==1])

#Q5

mean(disease[test==1])/mean(disease)

#Q6

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

#Q7

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#Q8

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y =(y), x =(x)) %>%
  qplot(x, y, data =.)

