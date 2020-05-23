library(dslabs)
mnist <- read_mnist()

train_images <- mnist$train$images
train_labels <- mnist$train$labels

x <- train_images[0:1000,]
y <- train_labels[0:1000]

x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

x4 <- train_images[4,]
y4 <- train_labels[4]
print (y4)

x7 <- train_images[7,]
y7 <- train_labels[7]
print (y7)

x8 <- train_images[8,]
y8<- train_labels[8]
print (y8)

print (sqrt(sum((x7-x4)^2)))
print (sqrt(sum((x8-x4)^2)))

print (sqrt(crossprod(x4-x7)))
print (sqrt(crossprod(x8-x7)))

d <- dist(x)
class (d)

dist_matrix <- as.matrix(d)
dim(dist_matrix)
dist_matrix[1:5,1:5]


#-----------------------------------------------------
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

x<- tissue_gene_expression$x
d <- dist(tissue_gene_expression$x)

print (sqrt(crossprod(x[1,]-x[2,])))
print (sqrt(crossprod(x[39,]-x[40,])))
print (sqrt(crossprod(x[73,]-x[74,])))
