##### Lab 5 - SVM Classification #####

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(reshape2)

## Function Declarations

identifyTableMetrics <- function(inTable) {
    n = sum(inTable) # number of instances
    nc = nrow(inTable) # number of classes
    diag = diag(inTable) # number of correctly classified instances per class 
    rowsums = apply(inTable, 1, sum) # number of instances per class
    colsums = apply(inTable, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the actual classes
    q = colsums / n # distribution of instances over the predicted 
    recall = diag / rowsums 
    precision = diag / colsums
    f1 = 2 * precision * recall / (precision + recall) 
    return(data.frame(precision, recall, f1))
}

# Import data
setwd("C:/Users/mlitc/Documents/Data_Analytics/lab4/")
wine <- read_csv("wine.data")
colnames(wine) <- c("class.wine", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid Phenols", "Proanthocyanins", "Color Intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
wine_class <- wine[, 1]

# Set up seed to ensure reproducability
set.seed(42)

# Standardize Data
wine_scaled <- scale(wine[, -1])

############ NEW STUFF ##############

apply(wine_scaled, 2, mean)
apply(wine_scaled, 1, sd)
wine_scaled <- as.data.frame(wine_scaled)
wine_scaled$wine.class <- factor(wine_class$class.wine)

# Training/Testing Data
trainingIndex <- sample(nrow(wine), 0.8 * nrow(wine))
trainingX <- wine_scaled[trainingIndex, ]
testingX <- wine_scaled[-trainingIndex, ]
trainingY <- wine_class[trainingIndex, ]
testingY <- wine_class[-trainingIndex, ]

# Boxplot
trainingXMelt <- melt(trainingX[, 1:13])
ggplot(trainingXMelt, aes(x = variable, y = value)) +
  geom_boxplot() +
  xlab("Wine Attributes") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### SVM Models ###

# SVM Model 1 - Linear Kernel
svm.mod1 <- svm(wine.class ~ Alcohol + Ash + `Alcalinity of ash` + Magnesium + `Total phenols` + 
    Flavanoids, data = trainingX, kernel = "linear")
svm.mod1
train.pred <- predict(svm.mod1, trainingX)
cm = as.matrix(table(Actual = trainingX$wine.class, Predicted = train.pred))
cm
svm_metrics <- identifyTableMetrics(cm)
svm_metrics

# SVM Model 2 - Radial Kernel

svm.mod2 <- svm(wine.class ~ Alcohol + Ash + `Alcalinity of ash` + Magnesium + `Total phenols` + 
    Flavanoids, data = trainingX, kernel = "radial")
svm.mod2
train.pred <- predict(svm.mod2, trainingX)
cm = as.matrix(table(Actual = trainingX$wine.class, Predicted = train.pred))
cm
svm_metrics <- identifyTableMetrics(cm)
svm_metrics

# Note: SVM Model 2 - Radial Kernel has better metrics

# Tuned.svm1 finds the optimal gamma and cost values through tune.svm
tuned.svm1 <- tune.svm(wine.class ~ Alcohol + Ash + `Alcalinity of ash` + Magnesium + `Total phenols` + 
    Flavanoids, data = trainingX, kernel = "radial", gamma = seq(1/2^nrow(trainingX), 1, .01), cost = 2^seq(-6, 4, 2))
tuned.svm1

# Predict using best gamma and cost values
best_model <- tuned.svm1$best.model
test.pred <- predict(best_model, testingX)
cm <- table(Actual = testingX$wine.class, Predicted = test.pred)
cm
svm_metrics <- identifyTableMetrics(cm)
svm_metrics


# Model 3 - KNN
knn.predicted <- knn(train = trainingX, test = trainingX, cl = trainingX$wine.class, k = 5)
contingency.table <- table(knn.predicted, trainingX$wine.class)
contingency.table
svm_metrics <- identifyTableMetrics(contingency.table)
svm_metrics