#Clustering
library(devtools)
library(devtools)
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
library(e1071)

#Q1
#Which of the following lines of code correctly does this computation?
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
d

#Q2
#Which tissue type is in the branch farthest to the left?
h <- hclust(d)
plot(h)

#Q3
#fill BLANK code
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)#Fill BLANK
