#Working With Matrices

#Q1
#x <- matrix(rnorm(1000), 100, 100)
x <- matrix(rnorm(100*10), 100, 10)
#x <- matrix(rnorm(100*10), 10, 10)
#x <- matrix(rnorm(100*10), 10, 100)

#Q2
dim(x)
nrow(x)
ncol(x)

#Q3
x <- matrix(0, 100, 10)
x <- x + seq(nrow(x))
x

x <- matrix(0, 100, 10)
x <- 1:nrow(x)
x

x <- matrix(0, 100, 10)
x <- sweep(x, 2, 1:nrow(x),"+")
x

x <- matrix(0, 100, 10)
x <- sweep(x, 1, 1:nrow(x),"+")
x

#Q4
x <- matrix(0, 100, 10)
x <- 1:ncol(x)
x

x <- matrix(0, 100, 10)
x <- 1:col(x)
x

x <- matrix(0, 100, 10)
x <- sweep(x, 2, 1:ncol(x), FUN = "+")
x

x <- matrix(0, 100, 10)
x <- -x
x

#Q5
x <- matrix(0, 100, 10)
x <- 1:col(x)
x

rowMeans(x)
colMeans(x)

#Q6
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images
y <- mnist$train$labels

ind <- (x > 50 & x < 205)
ind <- rowMeans(ind)
mean(ind)

#Q6 Answer from edX
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels