# Load necessary libraries
library(CIBERSORT)  
library(corrplot)
library(ggplot2)
library(pROC)


data <- read.csv("C:\Users\bhava\Downloads\12967_2020_2698_MOESM2_ESM.xls")

# Perform immune cell quantification using CIBERSORT

immune_cells <- CIBERSORT(data, "LM22.txt", perm=100)

# Check results and visualize the immune cell proportions
print(immune_cells$results)
barplot(as.matrix(immune_cells$results), col=rainbow(22), main="Immune Cell Proportions in SLE Patients")

# Correlation analysis among immune cell types
corr_matrix <- cor(immune_cells$results, use="pairwise.complete.obs")
corrplot(corr_matrix, method="color")

# PCA for clustering based on immune cell composition
pca_results <- prcomp(immune_cells$results, scale.=TRUE)
plot(pca_results$x[,1:2], col=c("red","blue")[factor(data$Condition)], pch=19, xlab="PC1", ylab="PC2")
legend("topright", legend=levels(factor(data$Condition)), fill=c("red","blue"))

# ROC analysis for biomarkers
# Assuming you have a dataframe `biomarkers` with actual outcomes and predicted probabilities
biomarkers <- read.csV("C:/Users/bhava/Downloads/12967_2020_2698_MOESM6_ESM.xls")
roc_result <- roc(biomarkers$response, biomarkers$score)

# Plot ROC curve
plot(roc_result, main="ROC Curve for SLE Biomarkers")
auc(roc_result)

# Print AUC value
print(auc(roc_result))

