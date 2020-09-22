#Titanic
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Question 1: Training and test sets
set.seed(42, sample.kind = 'Rounding') # if R version >= 3.6
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

#Question 2: Baseline prediction by guessing the outcome
set.seed(3, sample.kind = 'Rounding') # if R version >= 3.6
guess_ <- sample(c(0,1), nrow(test_set), replace = TRUE)
test_set %>% 
  filter(Survived == guess_) %>%
  summarize(n() / nrow(test_set))
# guess with equal probability of survival
# guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
# mean(guess == test_set$Survived)

#Question 3a: Predicting survival by sex
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1))
#or you can use
# train_set %>%
#   group_by(Sex) %>%
#   summarize(Survived = mean(Survived == 1)) %>%
#   filter(Sex == "male") %>%
#   pull(Survived)

#Question 3b: Predicting survival by sex
test_set %>%
  summarize( (sum(Sex == 'female' & Survived == 1) + sum(Sex == 'male' & Survived == 0)) / n())
#or you can use
# sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
# mean(sex_model == test_set$Survived)    # calculate accuracy

#Question 4a: Predicting survival by passenger class
survival_class <- titanic_clean %>%
  group_by(Pclass) %>%
  summarize(PredictingSurvival = ifelse(mean(Survived == 1) >=0.5, 1, 0))
survival_class

#Question 4b: Predicting survival by passenger class
test_set %>%
  inner_join(survival_class, by='Pclass') %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))

#Question 4c: Predicting survival by passenger class
survival_class <- titanic_clean %>%
  group_by(Sex, Pclass) %>%
  summarize(PredictingSurvival = ifelse(mean(Survived == 1) > 0.5, 1, 0))
survival_class
#or you can use
# train_set %>%
#   group_by(Sex, Pclass) %>%
#   summarize(Survived = mean(Survived == 1)) %>%
#   filter(Survived > 0.5)

#Question 4d: Predicting survival by passenger class
test_set %>%
  inner_join(survival_class, by=c('Sex', 'Pclass')) %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))
#or you can use
# sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
# mean(sex_class_model == test_set$Survived)

#Question 5a: Confusion matrix
# Confusion Matrix: sex model
library(broom)
sex_model <- train_set %>%
  group_by(Sex) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set1 <- test_set %>%
  inner_join(sex_model, by = 'Sex')
cm1 <- confusionMatrix(data = factor(test_set1$Survived_predict), reference = factor(test_set1$Survived))
cm1 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: class model
class_model <- train_set %>%
  group_by(Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set2 <- test_set %>%
  inner_join(class_model, by = 'Pclass')
cm2 <- confusionMatrix(data = factor(test_set2$Survived_predict), reference = factor(test_set2$Survived))
cm2 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: sex and class model
sex_class_model <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set3 <- test_set %>%
  inner_join(sex_class_model, by=c('Sex', 'Pclass'))
cm3 <- confusionMatrix(data = factor(test_set3$Survived_predict), reference = factor(test_set3$Survived))
cm3 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate

#Question 6: F1-Score
F_meas(data=factor(test_set1$Survived), reference = factor(test_set1$Survived_predict))
F_meas(data=factor(test_set2$Survived), reference = factor(test_set2$Survived_predict))
F_meas(data=factor(test_set3$Survived), reference = factor(test_set3$Survived_predict))
#or you can use
# F_meas(data = factor(sex_model), reference = test_set$Survived)
# F_meas(data = factor(class_model), reference = test_set$Survived)
# F_meas(data = factor(sex_class_model), reference = test_set$Survived)

#Question 7: Survival by fare - LDA and QDA
#LDA
fit_lda <- train(Survived ~ Fare, data = train_set, method = 'lda')
Survived_hat <- predict(fit_lda, test_set)
mean(test_set$Survived == Survived_hat)
#QDA
fit_qda <- train(Survived ~ Fare, data = train_set, method = 'qda')
Survived_hat <- predict(fit_qda, test_set)
mean(test_set$Survived == Survived_hat)
#or you can use#
#LDA
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)
#QDA
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean

#Question 8: Logistic regression models
#accuracy on the test set using age as the only predictor
fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)
#accuracy on the test set using these four predictors(sex, class, fare, and age)
fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
mean(survived_hat_b == test_set$Survived)
#accuracy on the test set using all predictors
str(train_set)
fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
mean(survived_hat_c == test_set$Survived)
#or you can use#
#Sex
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)
#Sex, Class, Fare and Age
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)
#All Predictor
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

#Question 9a: kNN model
set.seed(6, sample.kind = "Rounding")
# Method below doesn't give same result as EdX (though it is correct)
# ks <- seq(3,51,2)
# res_knn9a <- sapply(ks, function(k) {
#     fit_knn9a <- knn3(Survived ~ ., data = train_set, k = k)
#     survived_hat <- predict(fit_knn9a, train_set, type = "class") %>% factor(levels = levels(train_set$Survived))
#     cm_test <- confusionMatrix(data = survived_hat, reference = train_set$Survived)
#     cm_test$overall["Accuracy"]
# })
# ks[which.max(res_knn9a)]
# Other method using train function
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune
#or you can use#
#set.seed(6)
# set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
# train_knn <- train(Survived ~ .,
#                    method = "knn",
#                    data = train_set,
#                    tuneGrid = data.frame(k = seq(3, 51, 2)))
# train_knn$bestTune

#Question 9b: kNN model
ggplot(fit_knn9a)
#or if you are using edX code instead#
# ggplot(train_knn)

#Question 9c: kNN model
survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]
#or if you are using edX code instead#
# knn_preds <- predict(train_knn, test_set)
# mean(knn_preds == test_set$Survived)

#Question 10: Cross-validation
set.seed(8, sample.kind = "Rounding")
fit_knn10 <- train(Survived ~ ., 
                   data=train_set, 
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number=10, p=0.9))
fit_knn10
survived_hat <- predict(fit_knn10, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]
#or you can use
#set.seed(8)
# set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
# train_knn_cv <- train(Survived ~ .,
#                       method = "knn",
#                       data = train_set,
#                       tuneGrid = data.frame(k = seq(3, 51, 2)),
#                       trControl = trainControl(method = "cv", number = 10, p = 0.9))
# train_knn_cv$bestTune
# knn_cv_preds <- predict(train_knn_cv, test_set)
# mean(knn_cv_preds == test_set$Survived)

#Question 11a: Classification tree model
set.seed(10, sample.kind = 'Rounding')
fit_rpart11 <- train(Survived ~ ., 
                     data=train_set, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
plot(fit_rpart11)
survived_hat <- predict(fit_rpart11, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]
#or you can use
#set.seed(10)
# set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
# train_rpart <- train(Survived ~ ., 
#                      method = "rpart",
#                      tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
#                      data = train_set)
# train_rpart$bestTune
# rpart_preds <- predict(train_rpart, test_set)
# mean(rpart_preds == test_set$Survived)

#Question 11b: Classification tree model
fit_rpart11$finalModel
plot(fit_rpart11$finalModel, margin=0.1)
text(fit_rpart11$finalModel, cex = 0.75)
#or you can use
# train_rpart$finalModel # inspect final model
# 
# # make plot of decision tree
# plot(train_rpart$finalModel, margin = 0.1)
# text(train_rpart$finalModel)

#Question 12: Random forest model
set.seed(14, sample.kind = 'Rounding')
fit12_rf <- train(Survived ~., 
                  data = train_set,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = seq(1, 7)), 
                  ntree = 100)
fit12_rf$bestTune
survived_hat <- predict(fit12_rf, test_set)
mean(survived_hat == test_set$Survived)
varImp(fit12_rf)
#or you can use
##set.seed(14)
# set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
# train_rf <- train(Survived ~ .,
#                   data = train_set,
#                   method = "rf",
#                   ntree = 100,
#                   tuneGrid = data.frame(mtry = seq(1:7)))
# train_rf$bestTune
# rf_preds <- predict(train_rf, test_set)
# mean(rf_preds == test_set$Survived)
# varImp(train_rf)    # first row