#Ensembles
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", 
            "gamLoess", "multinom", "qda", "rf", "adaboost")

#Q1
#did you train all models ?
library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
#Q1 Answer = yes

#Q2
#number of rows and column
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

#Q3
#Report the mean accuracy across all models.
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#Q4
#What is the accuracy of the ensemble?
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5
#How many of the individual methods do better than the ensemble?
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

#Q6
#What is the mean of these training set accuracy estimates?
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#Q7
#What is the accuracy of the ensemble now?
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
