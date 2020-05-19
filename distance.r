set.seed(0,sample.kind="Rounding")
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,]
x_2 <- x[2.]
x_3 <- x[3,]
sqrt(sum((x_1-x_2)^2))

d <- dist(x)
class(d)
d

as.matrix(d)[1:3,1:3]

image(as.matrix(d))

image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))

library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression)
d <- dist(tissue_gene_expression$x)
image(as.matrix(d))
