##### Data Analytics - Assignment 6 #####
# Source for data: https://archive.ics.uci.edu/dataset/20/census+income

library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)
library(dplyr)
library(tidyverse)
library(reshape2)
library(GGally)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

### EDA ###

## Data cleaning ##

# Load in data files
setwd("C:/Users/mlitc/Documents/Data_Analytics/assignment6/")
train_data <- read.table(
  file = "census+income/adult.data",
  sep = ",",
  strip.white = TRUE
)
test_data <- read.table(
  file = "census+income/adult.test",
  sep = ",",
  skip = "1",
  strip.white = TRUE
)
# Column names grabbed from adult.names
col_names <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
)
numeric_cols <- c("age", "fnlwgt", "education_num",
                  "capital_gain", "capital_loss", "hours_per_week")
cat_cols <- c("workclass", "education", "marital_status", "occupation",
              "relationship", "race", "sex", "native_country", "income")
# Attaching column names to the datasets, since they were not in the files already
colnames(train_data) <- col_names
colnames(test_data) <- col_names

# Print info on each dataset
summary(train_data)
summary(test_data)

# Printing out the current number of NA values in the data (0 present)
na_count1 <- sum(is.na(train_data))
na_count2 <- sum(is.na(test_data))
print(paste("Train data NA value count: ", na_count1))
print(paste("Test data NA value count:", na_count2))
# Conversting "?" values to NA, since that is the convention for missing values in this data
train_data[train_data == "?"] <- NA
test_data[test_data == "?"] <- NA
# Printing out the current number of NA values in the data POST "?" to NA conversion
na_count1 <- sum(is.na(train_data))
na_count2 <- sum(is.na(test_data))
print(paste("Train data NA value count: ", na_count1))
print(paste("Test data NA value count:", na_count2))
# Removing all rows with NA values in the data
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)
# Printing out the current number of NA values in the data after removing the new NA values
na_count1 <- sum(is.na(train_data))
na_count2 <- sum(is.na(test_data))
print(paste("Train data NA value count: ", na_count1))
print(paste("Test data NA value count:", na_count2))

# Removing periods in the income section for test_data
test_data$income  <- gsub("\\.", "", test_data$income)

# Print info on each dataset after NA removal
summary(train_data)
summary(test_data)

# Creates a new dataset with no outliers in it when given a dataset and a variable in said dataset
removeOutliers <- function(currDataset) {
  for (currVariable in numeric_cols) {
    Q1 <- quantile(currDataset[[currVariable]], 0.25)
    Q3 <- quantile(currDataset[[currVariable]], 0.75)
    IQR_value <- IQR(currDataset[[currVariable]])
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    outputDataSet <- currDataset %>% filter(.data[[currVariable]] > lower_bound & .data[[currVariable]] < upper_bound)
  }
  return(outputDataSet)
}

train_data_no_outliers <- removeOutliers(train_data)
test_data_no_outliers <- removeOutliers(test_data)

# Print info on each dataset after outlier removal
summary(train_data_no_outliers)
summary(test_data_no_outliers)

# Take subset with 2500 random points from each dataset, as the originals are very large
set.seed(42)
train_data_subset <- train_data %>% slice_sample(n = 2500, replace = FALSE)
test_data_subset <- test_data %>% slice_sample(n = 2500, replace = FALSE)
train_data_subset_no_outliers <- train_data %>% slice_sample(n = 2500, replace = FALSE)
test_data_subset_no_outliers <- test_data %>% slice_sample(n = 2500, replace = FALSE)

## Plotting ##

selectData <- function(train, test, k) {
  # Combining test and training data for EDA only
  total_data <- rbind(train, test)
  
  # Density plots of numeric features by income
  numerical_cols_and_income <- total_data[, c(numeric_cols, "income")]
  numerical_cols_and_income_melt <- melt(numerical_cols_and_income, id.vars = "income")
  p <- ggplot(numerical_cols_and_income_melt, aes(x = value, color = income, fill = income)) +
    geom_density(alpha = 0.2) + facet_wrap(~variable, scales = "free") +
    theme_minimal() + ggtitle("Density plots of numeric features by income")
  print(p)
  
  # Frequency analysis for categporical variables
  for (currVariable in cat_cols) {
    p <- ggplot(total_data, aes_string(x = currVariable)) +
      geom_bar() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste("Bar Plot of", currVariable))
    print(p)
  }
  
  # Printing distribution of the income variable
  print("Class distribution (income):\n")
  print(table(total_data$income))
  prop.table(table(total_data$income))
  
  ### Model Development ###
  
  ## Classification Models (1-3)
  
  # Run the 3 classification models, Random Forest, KNN, and Rpart
  runClassificationModels <- function(currDataset) {
    # Cross validation method with 5 iterations
    control <- trainControl(method = "cv", number = 5)
    metric <- "Accuracy"
    
    # Random Forest Model with all features - shows what ones are not important
    mod.rf <- train(income~., data = currDataset, method = "rf", metric = metric, trControl = control)
    #print(mod.rf)
    imp <- varImp(mod.rf)$importance
    imp_sorted <- imp[order(-imp$Overall), , drop = FALSE]
    print(imp_sorted)
    # Rpart and KNN models with all features
    mod.rpart <- train(income~., data = currDataset, method = "rpart", metric = metric, trControl = control)
    mod.knn <- train(income~., data = currDataset, method = "knn", metric = metric, trControl = control)
    
    # See results (accuracy)
    results <- resamples(list(rf = mod.rf, rpart = mod.rpart, knn = mod.knn))
    print(summary(results))
  }
  
  # Run the 3 classification models, Random FOrest, KNN, and Rpart once
  runClassificationModels(train)
  
  # Removing features that were not useful in previous model versions
  train <- train %>% select(-native_country)
  train <- train %>% select(-marital_status)
  
  # Run the 3 classification models, Random FOrest, KNN, and Rpart after removing unncessary features
  runClassificationModels(train)
  
  ## Clustering Model (K-Means) ##
  
  # Scale numeric features
  cluster_scaled <- scale(train[, numeric_cols])
  
  # Performing PCA
  pca_res <- prcomp(cluster_scaled, center = TRUE, scale. = TRUE)
  pca_features <- as.data.frame(pca_res$x)
  pca_features$income <- train$income
  summary(pca_features)
  
  # Creating a 2D PCA visualization
  p <- fviz_pca_ind(pca_res,
               geom.ind = "point",
               pointshape = 19,
               pointsize = 2,
               col.ind = train$income) +
    scale_color_manual(values = c("<=50K" = "red", ">50K" = "blue")) +
    ggtitle("PCA Projection Colored by Income")
  print(p)
  
  # Elbow method for finding optimal kmeans value
  p <- fviz_nbclust(cluster_scaled, kmeans, method = "wss") +
    ggtitle("Elbow Method for Optimal K")
  print(p)
  
  # Kmeans model
  mod.kmeans <- kmeans(cluster_scaled, centers = k, nstart = 25)
  print(mod.kmeans)
  print(paste("Total within-cluster SS:", mod.kmeans$tot.withinss))
  
  # Add cluster label to data
  cluster_assignments <- as.factor(mod.kmeans$cluster)
  
  # Plot in PCA-reduced space
  p <- fviz_cluster(mod.kmeans, data = cluster_scaled,
               geom = "point",
               ellipse.type = "norm",
               ggtheme = theme_minimal()) +
    ggtitle("K-Means Clustering on Income Census")
  print(p)
  
  ## Classification being run again with pca
  runClassificationModels(pca_features)
}

selectData(train_data_subset, test_data_subset, 3)
selectData(train_data_subset_no_outliers, test_data_subset_no_outliers, 6)