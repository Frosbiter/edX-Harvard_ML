#Dimension Reduction
library(caret)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(devtools)
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
library(e1071)
library(dslabs)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Q1
#Which tissue is in a cluster by itself?
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q2
#What is the correlation?
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

#Q3
#Which line of code should be used to replace #BLANK in the code block?
library(dplyr)
library(ggplot2)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))# Fill BLANK
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4
#For the 7th PC, which two tissues have the greatest median difference?
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#Q5
#How many PCs are required to reach a cumulative percent variance explained greater than 50%?
plot(summary(pc)$importance[3,])
