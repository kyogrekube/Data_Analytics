# Libraries
library("ggplot2")
library("readr")
library(readr)
library(class)
library(caret)
library(dplyr)

# Set working directory, open csv, view csv
setwd("C:/Users/mlitc/Documents/Data_Analytics/assignment2/")
readCSV <- read_csv("epi_results_2024_pop_gdp.csv")
View(readCSV)

# Create variable for modified dataset and view it
modifiedData <- readCSV
View(modifiedData)

# Check for NAN and removes all rows with NAN present
for (col in c("population", "gdp", "EPI.new", "ECO.new", "BDH.new", "SPI.new", "BER.new", "RLI.new")) {
  NACounter <- sum(is.na(modifiedData[[col]]))
  if (NACounter > 0) {
    modifiedData <- modifiedData[!is.na(modifiedData[[col]]), ]
  }
}

### Variable Distributions ###

# Derive 2 subsets, each for a different region
subsetRegion1 <- subset(modifiedData, region == unique(modifiedData$region)[1])
subsetRegion2 <- subset(modifiedData, region == unique(modifiedData$region)[2])

# Define the common varirable to use in the plots between the 2 regions
commonVariable <- "ECO.new"

# Plot histograms with density lines overlayed for each region and display them
histogram1 <- ggplot(subsetRegion1, aes(x = ECO.new)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  ggtitle(paste("Histogram with Density -", unique(modifiedData$region)[1])) +
  theme_minimal()

histogram2 <- ggplot(subsetRegion2, aes(x = ECO.new)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "green", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  ggtitle(paste("Histogram with Density -", unique(modifiedData$region)[2])) +
  theme_minimal()

print(histogram1)
print(histogram2)

# Plot QQ Plot for the same variable between the 2 regions (1 QQ plot)
# MODIFY THIS BIT. IT SHOULD ONLY MAKE 1 QQ PLOT!

QQPlot1 <- ggplot(subsetRegion1, aes(sample = get(commonVariable))) +
  stat_qq(distribution = qnorm) +  # Compare against a normal distribution
  stat_qq_line(distribution = qnorm, color = "red") +
  ggtitle(paste("Q-Q Plot -", unique(modifiedData$region)[1])) +
  theme_minimal()

QQPlot2 <- ggplot(subsetRegion2, aes(sample = get(commonVariable))) +
  stat_qq(distribution = qnorm) +  # Compare against a normal distribution
  stat_qq_line(distribution = qnorm, color = "red") +
  ggtitle(paste("Q-Q Plot -", unique(modifiedData$region)[2])) +
  theme_minimal()

# Display plots
print(QQPlot1)
print(QQPlot2)

### Linear Models ###

# Choose a variable (ECO.new) and fit 2 linear models with that variable as response.
# Choose either population or GDP as predictors. Apply transformations if needed (e.g. log)
# to varirables to obtain the best performing model.
fullDatasetModel1 <- lm((ECO.new) ~ log10(gdp), data = modifiedData)
fullDatasetModel2 <- lm((ECO.new) ~ log10(population), data = modifiedData)

# Print model summary stats
summary(fullDatasetModel1)
summary(fullDatasetModel2)

# p value of fullDatasetModel1 (using log10(gdp) as predictor): 3.1773-08
# p value of fullDatasetModel2 (using log10(population) as predictor): 0.8222
# The more significant predictor is gdp because the p value is smaller

# Plot the most significant predictor vs. the response
fullDatasetPredictorPlot1 <- ggplot(modifiedData, aes(x = log10(gdp), y = ECO.new)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("ECO.new vs log10(GDP)") +
  theme_minimal()
print(fullDatasetPredictorPlot1)

fullDatasetPredictorPlot2 <- ggplot(modifiedData, aes(x = log10(population), y = ECO.new)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("ECO.new vs log10(Population)") +
  theme_minimal()
print(fullDatasetPredictorPlot2)

# Plot the residual
residuals1 <- data.frame(Fitted = fitted(fullDatasetModel1), Residuals = resid(fullDatasetModel1))
residuals2 <- data.frame(Fitted = fitted(fullDatasetModel2), Residuals = resid(fullDatasetModel2))
fullDataset1Residual <- ggplot(residuals1, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals Plot for fullDatasetModel1l") +
  theme_minimal()
fullDataset2Residual <- ggplot(residuals2, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals Plot for fullDatasetModel2") +
  theme_minimal()
print(fullDataset1Residual)
print(fullDataset2Residual)

# Repeat the previous models but with a subset of 1 region (southern asia) instead.

subRegionData1 <- lm((ECO.new) ~ log10(gdp), data = subsetRegion1)
subRegionData2 <- lm((ECO.new) ~ log10(population), data = subsetRegion1)

# Plot summaries of each model
summary(subRegionData1)
summary(subRegionData2)

# Plot the most significant predictor vs. the response
subRegionPredictorPlot1 <- ggplot(subsetRegion1, aes(x = log10(gdp), y = ECO.new)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("ECO.new vs log10(GDP)") +
  theme_minimal()
print(subRegionPredictorPlot1)

subRegionPredictorPlot2 <- ggplot(subsetRegion1, aes(x = log10(population), y = ECO.new)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("ECO.new vs log10(Population)") +
  theme_minimal()
print(subRegionPredictorPlot2)

# Plot the residual
residuals1 <- data.frame(Fitted = fitted(subRegionData1), Residuals = resid(subRegionData1))
residuals2 <- data.frame(Fitted = fitted(subRegionData2), Residuals = resid(subRegionData2))
subRegion1Residual <- ggplot(residuals1, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals Plot for subRegionData1") +
  theme_minimal()
subRegion2Residual <- ggplot(residuals2, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals Plot for subRegionData2") +
  theme_minimal()
print(subRegion1Residual)
print(subRegion2Residual)

### Classification (kNN) ###

# Define the two regions and create a subset using 3 varirables
KNNsubset <- modifiedData[modifiedData$region %in% c("Southern Asia", "Eastern Europe"), c("region", "EPI.new", "ECO.new", "BDH.new")]
KNNsubset$region <- as.factor(KNNsubset$region)

# Create training (80%) and testing (20%) data by splitting our subset
# Seed is used to prevent change upon runs.
set.seed(42)
trainingIndex <- sample(1 : nrow(KNNsubset), 0.8 * nrow(KNNsubset))
# X values hold the 3 variables (predictors) and Y values hold the "region" class label
trainingX <- KNNsubset[trainingIndex, ][, 2:4] 
trainingY <- KNNsubset[trainingIndex, ]$region
testingX <- KNNsubset[-trainingIndex, ][, 2:4]
testingY <- KNNsubset[-trainingIndex, ]$region

# Looping through k values until a k value is found with an accuracy of 1.
# The confusion matrix is printed for the k value that achieves this.
for (k in 1:10) {
  KNNVar <- knn(trainingX, testingX, trainingY, k = k)
  confusionMatrixVar <- confusionMatrix(KNNVar, testingY)
  accuracy <- sum(diag(confusionMatrixVar$table)) / sum(confusionMatrixVar$table)
  print(paste("k value=", k, "Accuracy =", accuracy))
  if (accuracy == 1) {
    print(confusionMatrixVar)
    break
  }
}

# Repeat previous model 3 other variables and the same k value

# Define the two regions and create a subset using 3 varirables
KNNsubset <- modifiedData[modifiedData$region %in% c("Southern Asia", "Eastern Europe"), c("region", "SPI.new", "BER.new", "RLI.new")]
KNNsubset$region <- as.factor(KNNsubset$region)

# Create training (80%) and testing (20%) data by splitting our subset
# Seed is used to prevent change upon runs.
trainingIndex <- sample(1 : nrow(KNNsubset), 0.8 * nrow(KNNsubset))
# X values hold the 3 variables (predictors) and Y values hold the "region" class label
trainingX <- KNNsubset[trainingIndex, ][, 2:4] 
trainingY <- KNNsubset[trainingIndex, ]$region
testingX <- KNNsubset[-trainingIndex, ][, 2:4]
testingY <- KNNsubset[-trainingIndex, ]$region

# K value of 3 is used since that is the value the previous model ended upon
KNNVar <- knn(trainingX, testingX, trainingY, k = 3)
confusionMatrixVar <- confusionMatrix(KNNVar, testingY)
accuracy <- sum(diag(confusionMatrixVar$table)) / sum(confusionMatrixVar$table)
print(paste("k value=", 3, "Accuracy =", accuracy))
print(confusionMatrixVar)