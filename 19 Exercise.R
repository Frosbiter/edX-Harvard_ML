#Comprehension Check : Regularization

# set.seed(1986) # if using R 3.5 or earlier
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
#What is the ID of the top school?
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

#Q2
#What is the median school size overall?
median(schools$size)
#What is the median school size of the of the top 10 schools based on the score?
schools %>% top_n(10, score) %>% .$size %>% median()

#Q3
#What is the median school size of the bottom 10 schools based on the score?
median(schools$size)
schools %>% top_n(-10, score) %>% .$size %>% median()

#Q4
# From this analysis, we see that the worst schools are also small. Plot the average score versus school size to see what's going on. Highlight the top 10 schools based on the true quality.
# What do you observe?
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 
#The standard error of the score has larger variability when the school is smaller, which is why both the best and the worst schools are more likely to be small.

#Q5
#What is the ID of the top school with regularization?
overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6
#What value of  ??  gives the minimum RMSE?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]  

#Q7
#Rank the schools based on the average obtained with the best  ??  from Q6. Note that no small school is incorrectly included.
#top ID and reg_avg_score 10th pos
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q8
#What value of  ??  gives the minimum RMSE here?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)] 
