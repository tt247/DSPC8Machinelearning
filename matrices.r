library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

length(x[,1])

x_1 <- 1:5
x_2 <- 6:10
cbind(x_1,x_2)

dim(x)

dim(x_1)
dim(as.matrix(x_1))

#converting a vector to a matrix

my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

mat_t <- matrix(my_vector, 3,5,byrow = TRUE)
mat_t

matrix(my_vector, 5, 5)

grid <- matrix(x[3,],28,28)
image(1:28,1:28,grid)

image(1:28, 1:28, grid[,28:1])

#Row and column summaries and apply

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x,1,mean)

sds <- apply(x,2,sd)

#Filtering columns based on summaries

library(matrixStats)
sds <- colSds(x)

qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[,28:1])

#extract columns and rows

x[,c(351,352)]

x[c(2,3),]

new_x <- x[,colSds(x)>60]
dim(new_x)

class(x[,1,drop=FALSE])
dim(x[,1,drop=FALSE])

#Indexing with matrices and binarizing the data

mat <- matrix(1:15, 5, 3)
mat
as.vector(mat)

qplot(as.vector(x), bins=30, color = I("black"))

new_x <- x
new_x[new_x <50] <-0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

#binarize the data

bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x> 255/2)*1

bin_X <- (x > 255/2)*1

#vectorization for matrices and matrix algebra operations

(x - rowMeans(x)) / rowSds(x)

t(t(x) - colMeans(x))

X_mean_0 <- sweep(x, 2, colMeans(x))

X_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(X_mean_0, 2, colSds(x), FUN = "/")

#Comprehension Check

x <- matrix(rnorm(100*10), 100, 10)
dim(x)
length(x[,1])
length(x[1,])


mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
