library(tidyverse)
library(dplyr)
library(dslabs)
library(randomForest)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#Q1
#Which code correctly performs this cross-validation?
# fit <- train(x_subset, y)
# fit$results

fit <- train(x_subset, y, method = "glm")
fit$results

# fit <- train(y, x_subset, method = "glm")
# fit$results
# 
# fit <- test(x_subset, y, method = "glm")
# fit$results


install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

#Q2
#Which of the following lines of code correctly creates a vector of the p-values called pvals?
# pvals <- tt$dm
# 
# pvals <- tt$statistic
# 
# pvals <- tt

pvals <- tt$p.value

#Q3
#How many predictors survive this cutoff?
ind <- which(pvals <= 0.01)
length(ind)

#Q4
#What is the accuracy now?
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

#Q5
#Make a plot of the resulting accuracies.
#Which code is correct?
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Q6
#In the previous exercises, we see that despite the fact that x and y are completely independent, we were able to predict y with accuracy higher than 70%. We must be doing something wrong then.
#What is it?
#We used the entire dataset to select the columns used in the model.

#Q7
#What value of k results in the highest accuracy?
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results