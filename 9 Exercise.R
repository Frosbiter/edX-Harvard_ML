library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

#Q1
d <- dist(tissue_gene_expression$x)

d <- dist(tissue_gene_expression$x, distance='maximum')
d <- dist(tissue_gene_expression)
d <- dist(tissue_gene_expression$x)#correct answer
d <- cor(tissue_gene_expression$x)

#Q2
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

#Q2 corret answer = Yes, the samples from the same tissue type are closer to each other.

#Q3
image(d)
image(as.matrix(d))#correct answer
d
image()
