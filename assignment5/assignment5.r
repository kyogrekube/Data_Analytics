#install.packages("e1071")
library(class)
library(ggplot2)
library(cluster)
library(tidyverse)
library(tidyr)
library(dplyr)
library(broom)
library(e1071)
library(caret)
library(ggfortify)
library(psych)
library(e1071)
library(randomForest)

## Function declarations

# Creates a scatterplot given a dataset, a variable in the dataset, and a string for the varirable
create_lm_plot <- function(currDataset, currVariable, currVariableTitle, outliersPresence) {
    outputDataset <- currDataset %>% filter(!is.na(SALE.PRICE) & !is.na(.data[[currVariable]])) 
    if (outliersPresence == TRUE) {
        plotTitle = paste("Sale Price vs", currVariableTitle, "in Manhattan (with outliers)")
    }
    else {
        plotTitle = paste("Sale Price vs", currVariableTitle, "in Manhattan (with NO outliers)")
    }
    p <- ggplot(outputDataset, aes(x = .data[[currVariable]], y = SALE.PRICE)) +
        geom_point() +
        geom_smooth(method="lm", color="green") + 
        ggtitle(plotTitle) +
        xlab(currVariableTitle) +
        ylab("Sale Price")
    print(p)
    return(outputDataset)
}
# Creates a scatterplot given a dataset, a variable in the dataset, a string for the varirable, and a boolean if outliers are present
create_histogram_plot <- function(currDataset, currVariable, currVariableTitle, outliersPresence) {
    outputDataset <- currDataset %>% filter(!is.na(.data[[currVariable]]))
    if (outliersPresence == TRUE) {
        plotTitle <- paste("Histogram of", currVariableTitle, "in Manhattan (with outliers)")
    }
    else {
        plotTitle <- paste("Histogram of", currVariableTitle, "in Manhattan (with NO outliers)")
    }
    hist(outputDataset[[currVariable]], 
        prob = TRUE, 
        main = plotTitle,
        xlab = currVariableTitle
    )
    lines(density(outputDataset[[currVariable]], na.rm = TRUE)) 
    return(outputDataset)
}

# Creates a new dataset with no outliers in it when given a dataset and a variable in said dataset
removeOutliers <- function(currDataset, currVariable) {
    Q1 <- quantile(currDataset[[currVariable]], 0.25)
    Q3 <- quantile(currDataset[[currVariable]], 0.75)
    IQR_value <- IQR(currDataset[[currVariable]])
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    outputDataSet <- currDataset %>% filter(.data[[currVariable]] > lower_bound & .data[[currVariable]] < upper_bound)
    return(outputDataSet)
}

identifyTableMetrics <- function(inTable) {
    n = sum(inTable) # number of instances
    nc = nrow(inTable) # number of classes
    diag = diag(inTable) # number of correctly classified instances per class 
    rowsums = apply(inTable, 1, sum) # number of instances per class
    colsums = apply(inTable, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the actual classes
    q = colsums / n # distribution of instances over the predicted 
    accuracy <- sum(diag)/n
    accuracy
    recall = diag / rowsums 
    precision = diag / colsums
    f1 = 2 * precision * recall / (precision + recall) 
    return(data.frame(precision, recall, f1))
}

# Setup working directory
setwd("C:/Users/mlitc/Documents/Data_Analytics/assignment5/")
nyc_data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# Removes commas from any of the below variables if they are in the values
nyc_data$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", nyc_data$GROSS.SQUARE.FEET))
nyc_data$SALE.PRICE <- as.numeric(gsub(",", "", nyc_data$SALE.PRICE))
nyc_data$LAND.SQUARE.FEET <- as.numeric(gsub(",", "", nyc_data$LAND.SQUARE.FEET))
nyc_data$YEAR.BUILT <- as.numeric(gsub(",", "", nyc_data$YEAR.BUILT))
nyc_data$ZIP.CODE <- as.numeric(gsub(",", "", nyc_data$ZIP.CODE))
summary(nyc_data)

# Filters data for only the borough of manhattan
manhattan_data <- nyc_data %>% filter(BOROUGH == "MANHATTAN")

# Set up seed to ensure reproducability
set.seed(42)

# Take subset with 2500 random points from each dataset, as the originals are very large
manhattan_data_subset <- manhattan_data %>% slice_sample(n = 2500, replace = FALSE)

### PART 1B -> EXPLORATORY ANALYSIS

## Exploratory Analysis on SALE.PRICE

# Create and display a histogram of all sales in Manhattan using SALE.PRICE with outliers
manhattan_sale_price <- create_histogram_plot(manhattan_data_subset, "SALE.PRICE", "Sale Price", TRUE)
# Remove outliers from SALE.PRICE
manhattan_sale_price_no_outliers <- removeOutliers(manhattan_sale_price, "SALE.PRICE")
# Create and display a histogram of all sales in Manhattan using SALE.PRICE with NO outliers
create_histogram_plot(manhattan_sale_price_no_outliers, "SALE.PRICE", "Sale Price", FALSE)

## Exploratory Analysis on GROSS.SQUARE.FEET and LAND.SQUARE.FEET
    
# Create and display a histogram of all sales in Manhattan using GROSS.SQUARE.FEET with outliers
create_histogram_plot(manhattan_data_subset, "GROSS.SQUARE.FEET", "Gross Square Feet", TRUE)
create_histogram_plot(manhattan_data_subset, "LAND.SQUARE.FEET", "Land Square Feet", TRUE)
# Create and display a scatterplot
manhattan_gross_square_feet <- create_lm_plot(manhattan_data_subset, "GROSS.SQUARE.FEET", "Gross Square Feet", TRUE)
manhattan_land_square_feet <- create_lm_plot(manhattan_data_subset, "LAND.SQUARE.FEET", "Land Square Feet", TRUE)
# Remove outliers from GROSS.SQUARE.FEET and LAND.SQUARE.FEET
manhattan_gross_square_feet_no_outliers <- removeOutliers(manhattan_gross_square_feet, "GROSS.SQUARE.FEET")
manhattan_land_square_feet_no_outliers <- removeOutliers(manhattan_land_square_feet, "LAND.SQUARE.FEET")
# Create and display a histogram of all sales in Manhattan using GROSS.SQUARE.FEET with no outliers
create_histogram_plot(manhattan_gross_square_feet_no_outliers, "GROSS.SQUARE.FEET", "Gross Square Feet", FALSE)
create_histogram_plot(manhattan_land_square_feet_no_outliers, "LAND.SQUARE.FEET", "Land Square Feet", FALSE)
# Create and display a scatterplot
create_lm_plot(manhattan_gross_square_feet_no_outliers, "GROSS.SQUARE.FEET", "Gross Square Feet", FALSE)
create_lm_plot(manhattan_land_square_feet_no_outliers, "LAND.SQUARE.FEET", "Land Square Feet", FALSE)

#Removing variables and cleanup from memory:
rm(manhattan_sale_price)
rm(manhattan_gross_square_feet)
rm(manhattan_land_square_feet)
rm(manhattan_sale_price_no_outliers)
rm(manhattan_gross_square_feet_no_outliers)
rm(manhattan_land_square_feet_no_outliers)

### PART 1C -> REGRESSION ANALYSIS

# Filtering new subset for the GROSS.SQUARE.FEET, LAND.SQUARE.FEET, SALE.PRICE, and YEAR.BUILT where there are no "na" values
manhattan_data_subset_filtered <- manhattan_data_subset %>%
    filter(!is.na(GROSS.SQUARE.FEET),
           !is.na(LAND.SQUARE.FEET),
           !is.na(SALE.PRICE),
           !is.na(YEAR.BUILT),
           !is.na(ZIP.CODE))

## Linear models, summaries, and plots

# Extra variables are GROSS.SQUARE.FEET, LAND.SQUARE.FEET, YEAR.BUILT, ZIP.CODE
lin.mod.manhattan <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT + ZIP.CODE, data = manhattan_data_subset_filtered)
summary(lin.mod.manhattan)
plot(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT + ZIP.CODE, data = manhattan_data_subset_filtered, main = "Linear Model of Manhattan Property Sales (All stats)")

# Extra variables are GROSS.SQUARE.FEET, LAND.SQUARE.FEET, YEAR.BUILT (no ZIP.CODE)
lin.mod.manhattan <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT, data = manhattan_data_subset_filtered)
summary(lin.mod.manhattan)
plot(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT, data = manhattan_data_subset_filtered, main = "Linear Model of Manhattan Property Sales (No Zipcode)")

# Extra variables are GROSS.SQUARE.FEET, LAND.SQUARE.FEET, ZIP.CODE (no YEAR.BUILT)
lin.mod.manhattan <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + ZIP.CODE, data = manhattan_data_subset_filtered)
summary(lin.mod.manhattan)
plot(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + ZIP.CODE, data = manhattan_data_subset_filtered, main = "Linear Model of Manhattan Property Sales (No Year built)")

### PART 1D -> TRAIN 3 UNSUPERVISED LEARNING MODELS

## Making presets

# Removing "na" from "neighborhood" column
manhattan_data_subset_filtered <- manhattan_data_subset_filtered %>% filter(!is.na(NEIGHBORHOOD))

# Create a subset using the 5 variables
manhattan_data_subset_filtered <- manhattan_data_subset_filtered[, 
    c("NEIGHBORHOOD", "GROSS.SQUARE.FEET", "LAND.SQUARE.FEET", "SALE.PRICE", "YEAR.BUILT", "ZIP.CODE")]
manhattan_data_subset_filtered$NEIGHBORHOOD <- as.factor(manhattan_data_subset_filtered$NEIGHBORHOOD)

# Create training (80%) and testing (20%) data by splitting our subset
trainingIndex <- sample(nrow(manhattan_data_subset_filtered), 0.8 * nrow(manhattan_data_subset_filtered))
train <- manhattan_data_subset_filtered[trainingIndex, ]
test <- manhattan_data_subset_filtered[-trainingIndex, ]
# X values hold the variables (predictors) and Y values hold the "neighborhood" class label
trainingX <- train[, -1] 
trainingY <- train$NEIGHBORHOOD
testingX <- test[, -1]
testingY <- test$NEIGHBORHOOD

## KNN supervised learning model
train_scaled <- scale(trainingX)
test_scaled <- scale(testingX, center = attr(train_scaled, "scaled:center"), scale = attr(train_scaled, "scaled:scale"))
KNNVar <- knn(train_scaled, test_scaled, trainingY, k = 3)
confusionMatrixVar <- confusionMatrix(KNNVar, testingY)
accuracy <- sum(diag(confusionMatrixVar$table)) / sum(confusionMatrixVar$table)
print(paste("k value=", 3, "Accuracy =", accuracy))
print(confusionMatrixVar)

## SVM supervised learning model
svm.mod0 <- svm(NEIGHBORHOOD ~ SALE.PRICE + GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT + ZIP.CODE, data = train, kernel = 'linear')
svm.mod0
plot(svm.mod0, data = train, formula = GROSS.SQUARE.FEET ~ LAND.SQUARE.FEET, svSymbol = "x", dataSymbol = "o")
train.pred <- predict(svm.mod0, train)
cm = as.matrix(table(Actual = train$NEIGHBORHOOD, Predicted = train.pred, dnn=list('predicted','actual')))
cm
svm_metrics <- identifyTableMetrics(cm)
svm_metrics

# Random forest supervised learning model
train$NEIGHBORHOOD <- droplevels(train$NEIGHBORHOOD)
test$NEIGHBORHOOD <- factor(test$NEIGHBORHOOD, levels = levels(train$NEIGHBORHOOD))
random_forest_model <- randomForest(NEIGHBORHOOD ~ ., data = train, ntree = 300)
rf_predicted <- predict(random_forest_model, testingX)
rf_predicted <- factor(rf_predicted, levels = levels(test$NEIGHBORHOOD))
rf_table <- table(Predicted = rf_predicted, Actual = test$NEIGHBORHOOD, dnn=list('predicted','actual'))
rf_table
rf_metrics <- identifyTableMetrics(rf_table)
rf_metrics

#Removing variables and cleanup from memory:
rm(manhattan_data)
rm(manhattan_data_subset)
rm(manhattan_data_subset_filtered)
rm(lin.mod.manhattan)

### Questions 2a - 2b

# Filters data for only the borough of staten island
staten_island_data <- nyc_data %>% filter(BOROUGH == "STATEN ISLAND")

# Take subset with 2500 random points from each dataset, as the originals are very large
staten_island_data_subset <- staten_island_data %>% slice_sample(n = 2500, replace = FALSE)

### PART 2A -> APPLY BEST PERFORMING REGRESSION MODEL FROM 1C TO NEW DATASET

# Filtering new subset for the GROSS.SQUARE.FEET, LAND.SQUARE.FEET, SALE.PRICE, and YEAR.BUILT where there are no "na" values
staten_island_data_subset_filtered <- staten_island_data_subset %>%
    filter(!is.na(GROSS.SQUARE.FEET),
           !is.na(LAND.SQUARE.FEET),
           !is.na(SALE.PRICE),
           !is.na(YEAR.BUILT),
           !is.na(ZIP.CODE))

# Extra variables are GROSS.SQUARE.FEET, LAND.SQUARE.FEET, YEAR.BUILT, ZIP.CODE
lin.mod.staten_island <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT + ZIP.CODE, data = staten_island_data_subset_filtered)
summary(lin.mod.staten_island)
plot(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT + ZIP.CODE, data = staten_island_data_subset_filtered, main = "Linear Model of Staten Island Property Sales (All stats)")

### PART 2B -> APPLY THE BEST CLASSIFICATION MODEL FROM 1D

## Making presets

# Removing "na" from "neighborhood" column
staten_island_data_subset_filtered <- staten_island_data_subset_filtered %>% filter(!is.na(NEIGHBORHOOD))

# Create a subset using the 5 variables
staten_island_data_subset_filtered <- staten_island_data_subset_filtered[, 
    c("NEIGHBORHOOD", "GROSS.SQUARE.FEET", "LAND.SQUARE.FEET", "SALE.PRICE", "YEAR.BUILT", "ZIP.CODE")]
staten_island_data_subset_filtered$NEIGHBORHOOD <- as.factor(staten_island_data_subset_filtered$NEIGHBORHOOD)

# Create training (80%) and testing (20%) data by splitting our subset
trainingIndex <- sample(nrow(staten_island_data_subset_filtered), 0.8 * nrow(staten_island_data_subset_filtered))
train <- staten_island_data_subset_filtered[trainingIndex, ]
test <- staten_island_data_subset_filtered[-trainingIndex, ]
# X values hold the variables (predictors) and Y values hold the "neighborhood" class label
trainingX <- train[, -1] 
trainingY <- train$NEIGHBORHOOD
testingX <- test[, -1]
testingY <- test$NEIGHBORHOOD

## KNN supervised learning model
train_scaled <- scale(trainingX)
test_scaled <- scale(testingX, center = attr(train_scaled, "scaled:center"), scale = attr(train_scaled, "scaled:scale"))
KNNVar <- knn(train_scaled, test_scaled, trainingY, k = 3)
confusionMatrixVar <- confusionMatrix(KNNVar, testingY)
accuracy <- sum(diag(confusionMatrixVar$table)) / sum(confusionMatrixVar$table)
print(paste("k value=", 3, "Accuracy =", accuracy))
print(confusionMatrixVar)

## SVM supervised learning model
svm.mod0 <- svm(NEIGHBORHOOD ~ SALE.PRICE + GROSS.SQUARE.FEET + LAND.SQUARE.FEET + YEAR.BUILT + ZIP.CODE, data = train, kernel = 'linear')
svm.mod0
plot(svm.mod0, data = train, formula = GROSS.SQUARE.FEET ~ LAND.SQUARE.FEET, svSymbol = "x", dataSymbol = "o")
train.pred <- predict(svm.mod0, train)
cm = as.matrix(table(Actual = train$NEIGHBORHOOD, Predicted = train.pred, dnn=list('predicted','actual')))
cm
svm_metrics <- identifyTableMetrics(cm)
svm_metrics

# Random forest supervised learning model
train$NEIGHBORHOOD <- droplevels(train$NEIGHBORHOOD)
test$NEIGHBORHOOD <- factor(test$NEIGHBORHOOD, levels = levels(train$NEIGHBORHOOD))
random_forest_model <- randomForest(NEIGHBORHOOD ~ ., data = train, ntree = 300)
rf_predicted <- predict(random_forest_model, testingX)
rf_predicted <- factor(rf_predicted, levels = levels(test$NEIGHBORHOOD))
rf_table <- table(Predicted = rf_predicted, Actual = test$NEIGHBORHOOD, dnn=list('predicted','actual'))
rf_table
rf_metrics <- identifyTableMetrics(rf_table)
rf_metrics