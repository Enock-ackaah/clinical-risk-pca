############################################################
# 1. Load Dataset
############################################################

data <- read.csv("C:/Users/enock_p22oyv9/OneDrive/Desktop/PCA PROJECTS/diabetes.csv")


############################################################
# 2. Descriptive Statistics
############################################################

# Summary statistics for all variables
summary(data)

# Mean for each numeric variable
sapply(data, mean, na.rm = TRUE)

# Standard deviation for each numeric variable
sapply(data, sd, na.rm = TRUE)

# Correlation matrix (predictors only)
cor(subset(data, select = -Outcome), use = "complete.obs")


############################################################
# 3. Histograms for all predictor variables
############################################################

# Remove Outcome variable
predictors <- subset(data, select = -Outcome)

# Set plotting layout (3 rows, 3 columns)
par(mfrow = c(3, 3))

# Loop to generate histograms
for (i in 1:ncol(predictors)) {
  hist(predictors[[i]],
       main = paste("Histogram of", colnames(predictors)[i]),
       xlab = colnames(predictors)[i],
       col = "skyblue",
       border = "black")
}

# Reset layout
par(mfrow = c(1, 1))


############################################################
# 4. Prepare Data for PCA
############################################################

# Remove Outcome variable for PCA
X <- subset(data, select = -Outcome)

# Standardize the predictor variables
X_scaled <- scale(X)


############################################################
# 5. Run PCA
############################################################

pca_res <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# View PCA results
summary(pca_res)        # Variance explained
pca_res$rotation        # Loadings (contribution of variables)


############################################################
# 6. PCA Plots
############################################################

# Scree plot
plot(pca_res, type = "l", main = "Scree Plot")

# Add PCA scores and Outcome label
scores <- as.data.frame(pca_res$x)
scores$Outcome <- factor(data$Outcome)

# PC1 vs PC2 plot colored by diabetes status
plot(scores$PC1, scores$PC2,
     col = ifelse(scores$Outcome == 1, "red", "blue"),
     pch = 19,
     xlab = "PC1",
     ylab = "PC2",
     main = "PC1 vs PC2 Colored by Diabetes Status")

legend("topright",
       legend = c("Non-Diabetic", "Diabetic"),
       col = c("blue", "red"),
       pch = 19)
# Run PCA
pca_res <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Calculate eigenvalues
eigenvalues <- (pca_res$sdev)^2
eigenvalues

# Apply Kaiser Criterion: keep PCs with eigenvalue > 1
kaiser_selected <- eigenvalues[eigenvalues > 1]
kaiser_selected

# Number of PCs to keep
length(kaiser_selected)
