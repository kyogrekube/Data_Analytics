##### Lab 6 - Error Estimation for Regression Models #####

library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)


# Load CSV
setwd("C:/Users/mlitc/Documents/Data_Analytics/lab6/")
nyc_data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")
# Convert to numeric by removing commas
nyc_data$SALE.PRICE <- as.numeric(gsub(",", "", nyc_data$SALE.PRICE))
nyc_data$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", nyc_data$GROSS.SQUARE.FEET))
# Keep only rows with valid values
nyc_data <- nyc_data %>% filter(!is.na(SALE.PRICE), !is.na(GROSS.SQUARE.FEET), SALE.PRICE > 0, GROSS.SQUARE.FEET > 0)
# Create seed for reproducibility
set.seed(42)
# Take subset with 5000 random points from each dataset, as the originals are very large
nyc_data_subset <- nyc_data %>% slice_sample(n = 5000, replace = FALSE)

## Plot dataset
ggplot(nyc_data_subset, aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point()

ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point()

## Linear Regression Model
lin.mod0 <- lm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset)
summary(lin.mod0)
ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

ggplot(lin.mod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## Cleaning data
nyc_data_subset.sub0 <- nyc_data_subset[-which(nyc_data_subset$GROSS.SQUARE.FEET==2184.207862 | nyc_data_subset$SALE.PRICE>20000000),]

## linear model with clean subset
lin.mod1 <- lm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset.sub0)
summary(lin.mod1)

ggplot(nyc_data_subset.sub0, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## SVM - linear

svm.mod0 <- svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset.sub0, kernel="linear")

summary(svm.mod0)

svm.pred0 <- data.frame(real = log10(nyc_data_subset.sub0$SALE.PRICE),predicted=predict(svm.mod0, nyc_data_subset.sub0))

ggplot(nyc_data_subset.sub0, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(GROSS.SQUARE.FEET), y=svm.pred0$predicted), col="green")

ggplot(svm.pred0, aes(x = predicted, y = real-predicted)) +
  geom_point() +
  geom_hline(yintercept = 0)