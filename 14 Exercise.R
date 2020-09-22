#Comprehension Check: Trees and Random Forests
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#Q1
#Which code correctly uses rpart() to fit a regression tree and saves the result to fit?
fit <- rpart(y ~ ., data = dat)

#Q2
#Which of the following plots has the same tree shape obtained in Q1?
plot(fit)
text(fit)

#Q3
#Below is most of the code to make a scatter plot of y versus x along with the predicted values based on the fit.
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2) #the answer

#Q4
#What code should replace #BLANK in the provided code?
library(randomForest)
fit <- randomForest(y ~ x, data = dat) #Previously #BLANK 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
#Q5
#Which of these graphs is produced by plotting the random forest?
plot(fit)

#Q6
#What code should replace #BLANK in the provided code?
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25) #Previously #BLANK
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

