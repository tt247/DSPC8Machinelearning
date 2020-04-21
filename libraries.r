
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)

data(heights)
data(reported_heights)

# needed to resolve error e1071 with Caret package
install.packages('e1071', dependencies=TRUE)
