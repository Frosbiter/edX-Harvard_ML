#Comprehension Check: Recommendation Systems
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

#Q1
#What year has the highest median number of ratings?
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q2
#What is the average rating for the movie The Shawshank Redemption?
#What is the average number of ratings per year for the movie Forrest Gump?
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#Q3
#What type of trend do you observe?
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#Q4
#Given your observations in the exercise in Q3, which of the following strategies would be most appropriate?
library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp)) #answer

#Q5
#What type of trend do you observe?
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#Q6
#Compute the average rating for each week and plot this average against date. Hint: use the round_date() function before you group_by().
#What type of trend do you observe?
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
#There is some evidence of a time effect on average rating.

#Q7
#If we define  du,i  as the day for user's  u  rating of movie  i , which of the following models is most appropriate?
#Yu,i=??+bi+bu+f(du,i)+??u,i , with  f  a smooth function of  du,i

#Q8
#Which genre has the lowest average rating?
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q9
#If we define  gu,i  as the genre for user  u 's rating of movie  i , which of the following models is most appropriate?
#Yu,i=??+bi+bu+???Kk=1xku,i??k+??u,i , with  xku,i=1  if  gu,i  is genre  k