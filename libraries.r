
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
library(HistData)
library(matrixStats)
library(genefilter)

data(heights)
data(reported_heights)

# needed to resolve error e1071 with Caret package
install.packages('e1071', dependencies=TRUE)

#Bio packages
install.packages("BiocManager")
BiocManager::install("genefilter")
