library(dslabs)
mnist <- read_mnist()

train_images <- mnist$train$images
train_labels <- mnist$train$labels

class (train_images)
dim(train_images)
train_images[1,]
train_images[60000,]

class (train_labels)
train_labels[0]

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

image(dist_matrix)