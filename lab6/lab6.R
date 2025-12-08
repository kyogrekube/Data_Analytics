##### Lab 6 - Error Estimation for Regression Models #####

library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)
library(dplyr)


# Load CSV
setwd("C:/Users/mlitc/Documents/Data_Analytics/lab6/")
nyc_data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")
# Only keep SALE.PRICE and GROSS.SQUARE.FEET columns
nyc_data <- nyc_data %>% select(SALE.PRICE, GROSS.SQUARE.FEET)
# Keep only rows with valid values
nyc_data <- nyc_data %>% filter(!is.na(SALE.PRICE), !is.na(GROSS.SQUARE.FEET), SALE.PRICE > 0, GROSS.SQUARE.FEET > 0)
# Convert to numeric by removing commas
nyc_data$SALE.PRICE <- as.numeric(gsub(",", "", nyc_data$SALE.PRICE))
nyc_data$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", nyc_data$GROSS.SQUARE.FEET))
# Create seed for reproducibility
set.seed(42)
# Take subset with 2500 random points from each dataset, as the originals are very large
nyc_data_subset <- nyc_data %>% slice_sample(n = 2500, replace = FALSE)

## Plot dataset
ggplot(nyc_data_subset, aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point()

ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point()

## Linear Regression Model
lin.mod0 <- lm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset)
ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

ggplot(lin.mod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## SVM - linear

svm.mod0 <- svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset, kernel="linear")
svm.pred0 <- data.frame(real = log10(nyc_data_subset$SALE.PRICE),predicted=predict(svm.mod0, nyc_data_subset))

ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(GROSS.SQUARE.FEET), y=svm.pred0$predicted), col="green")

ggplot(svm.pred0, aes(x = predicted, y = real-predicted)) +
  geom_point() +
  geom_hline(yintercept = 0)

## SVM - radial
svm.mod1 <- svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset, kernel="radial")
svm.pred1 <- data.frame(real = log10(nyc_data_subset$SALE.PRICE),predicted=predict(svm.mod1, nyc_data_subset))

ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(GROSS.SQUARE.FEET), y=svm.pred1$predicted), col="red")

ggplot(svm.pred1, aes(x = predicted, y = real-predicted)) +
  geom_point() +
  geom_hline(yintercept = 0)

## SVM - radial - optimized
gamma.range <- 10^seq(-3,2,1)
gamma.range

C.range <- 10^seq(-3,2,1)
C.range

# Used only 1,000 for tuned dataset because it was taking too long
tuned_dataset <- nyc_data_subset %>% slice_sample(n = 1000, replace = FALSE)

tuned.svm <- tune.svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), data=tuned_dataset, kernel="radial", gamma = gamma.range, cost = C.range, tune.control=tune.control(cross = 5))
tuned.svm

opt.gamma <- tuned.svm$best.parameters$gamma
opt.C <- tuned.svm$best.parameters$cost

svm.mod2 <- svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), nyc_data_subset, kernel="radial", gamma=opt.gamma, cost=opt.C)
svm.pred2 <- data.frame(real = log10(nyc_data_subset$SALE.PRICE),predicted=predict(svm.mod2, nyc_data_subset))

ggplot(nyc_data_subset, aes(x = log10(GROSS.SQUARE.FEET), y = log10(SALE.PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(GROSS.SQUARE.FEET), y=svm.pred2$predicted), col="red")

ggplot(svm.pred2, aes(x = predicted, y = real-predicted)) +
  geom_point() +
  geom_hline(yintercept = 0)

## Estimating Model Errors

## split train/test
train.indexes <- sample(nrow(nyc_data_subset),0.75*nrow(nyc_data_subset))

train <- nyc_data_subset[train.indexes,]
test <- nyc_data_subset[-train.indexes,]

### Cross Validation ###

### Monte Carlo CV

## Lin Model
k = 100
mae0 <- c()
mse0 <- c()
rmse0 <- c()

for (i in 1:k) {
  
  train.indexes <- sample(nrow(nyc_data_subset),0.75*nrow(nyc_data_subset))
  
  train <- nyc_data_subset[train.indexes,]
  test <- nyc_data_subset[-train.indexes,]
  
  lin.mod <- lm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), train)
  
  lm.pred <- predict(lin.mod, test)  
  
  err <- lm.pred-log10(test$SALE.PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae0 <- c(mae0,mean.abs.err)
  mse0 <- c(mse0,mean.sq.err)
  rmse0 <- c(rmse0,root.mean.sq.err)
}


results0 <- data.frame(mae=mean(mae0), mse=mean(mse0), rmse=mean(rmse0))
results0

## Linear SVM Model
k = 100
mae1 <- c()
mse1 <- c()
rmse1 <- c()

for (i in 1:k) {
  train.indexes <- sample(nrow(nyc_data_subset),0.75*nrow(nyc_data_subset))
  
  train <- nyc_data_subset[train.indexes,]
  test <- nyc_data_subset[-train.indexes,]
  
  svm.mod <- svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), train, kernel="linear")
  
  svm.pred <- predict(svm.mod, test)  
  
  err <- svm.pred-log10(test$SALE.PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae1 <- c(mae1,mean.abs.err)
  mse1 <- c(mse1,mean.sq.err)
  rmse1 <- c(rmse1,root.mean.sq.err)
}

mean(mae1)
mean(mse1)
mean(rmse1)

results1 <- data.frame(mae=mean(mae1), mse=mean(mse1), rmse=mean(rmse1))
results1

## Radial SVM Model
k = 100
mae2 <- c()
mse2 <- c()
rmse2 <- c()

for (i in 1:k) {
  train.indexes <- sample(nrow(nyc_data_subset),0.75*nrow(nyc_data_subset))
  
  train <- nyc_data_subset[train.indexes,]
  test <- nyc_data_subset[-train.indexes,]
  
  svm.mod <- svm(log10(SALE.PRICE) ~ log10(GROSS.SQUARE.FEET), train, kernel="radial", gamma=opt.gamma, cost=opt.C)
  
  svm.pred <- predict(svm.mod, test)  
  
  err <- svm.pred-log10(test$SALE.PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae2 <- c(mae2,mean.abs.err)
  mse2 <- c(mse2,mean.sq.err)
  rmse2 <- c(rmse2,root.mean.sq.err)
}

mean(mae2)
mean(mse2)
mean(rmse2)

results2 <- data.frame(mae=mean(mae2), mse=mean(mse2), rmse=mean(rmse2))
results2

results <- rbind(results0,results1,results2)
results