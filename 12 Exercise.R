#bootstrap
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

#Q1
#How many times do 3, 4, and 7 appear in the first resampled index?
#Enter the number of times 3 appears:

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

#Q2
#What is the total number of times that 3 appears in all of the resampled indexes?
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

#Q3
#Mean(Expected Error) and Standart Deviation(Standard Error)
#set.seed(1, sample.kind='rounding')
y <- rnorm(100, 0, 1)

# set.seed(1) # # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
sd(q_75)

#Q4
#Expected Value and Standard Error
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Q5
#same as Q4 with 10000 bootstrap samples instead of 10
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Q6
#When doing bootstrap sampling, the simulated samples are drawn from the empirical distribution of the original data.
#True or False: The bootstrap is particularly useful in situations when we do not have access to the distribution or it is unknown.
#True