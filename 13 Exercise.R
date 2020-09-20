#Generative Models
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Q1
#What is the accuracy? (LDA)
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

#Q2
#Which TWO genes appear to be driving the algorithm (i.e. the two genes with the highest means)?
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# predictor Name:
# - PLCB1
# - RAB1B [X]
# - MSH4
# - OAZ2 [X]
# - SPI1
# - SAPCD1
# - HEMK1

#Q3
#same as Q1, but with QDA Accuracy
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]

#Q4
#Which TWO genes drive the algorithm when using QDA instead of LDA (i.e. the two genes with the highest means)?
t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Predictor Name:
# - PLCB1
# - RAB1B [X]
# - MSH4
# - OAZ2 [X]
# - SPI1
# - SAPCD1
# - HEMK1

#Q5
#Which TWO genes drive the algorithm after performing the scaling?
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

#Answer
# - C21orf62
# - PLCB1
# - RAB1B
# - MSH4
# - OAZ2 [X]
# - SPI1 [X]
# - SAPCD1
# - IL18R1

#Q6
#What is the accuracy using LDA?
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
fit_lda$results["Accuracy"]
