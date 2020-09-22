#Caret Package
#Q1
#Which value of cp gives the highest accuracy?
library(caret)
library(dslabs)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit1 <- with(tissue_gene_expression, 
             train(x, y, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit1)

#Q2
#What is the accuracy now?
library(caret)                    
library(rpart)
library(dslabs)
data("tissue_gene_expression")

# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

#Q3
#Plot the tree from the best fitting model of the analysis you ran in Q2.
#Which gene is at the first split?
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

#Q4
#What value of mtry maximizes accuracy?
# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

#Q5
#use the function varImp() on the output of train() and save it to an object called imp:
imp <- varImp(fit) ## FILL MISSING CODE
imp

#Q6
#What is the importance of the CFHR4 gene in the Random Forest call?
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
#What is the rank of the CFHR4 gene in the Random Forest call?
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)