####### Data Analytics Fall 2025 Lab 2 ######

library(ggplot2)

### Set working directory
setwd("C:/Users/mlitc/Documents/Data_Analytics/lab2/")

### Read in data
house.data <- read.csv("NY-House-Dataset.csv", header = TRUE)

View(house.data)

### Clean data ###

## Remove NA instances
house.clean <- na.omit(house.data)

## Keep relevant variables only
house.df <- data.frame(Price = house.clean$PRICE, PropertySqFt = house.clean$PROPERTYSQFT, Beds = house.clean$BEDS, Bath = house.clean$BATH
)

## Rounding numbers to integers
house.df$Bath <- round(house.df$Bath)

## Remove outliers
meanVal <- mean(house.df$Price, na.rm = TRUE)
sdVal <- sd(house.df$Price, na.rm = TRUE)
house.df <- house.df[abs(house.df$Price - meanVal) <= 3*sdVal, ]

#### Make 3 different models ####

### Model 0: All predictors
lin.mod0 <- lm(Price ~ PropertySqFt + Beds + Bath, house.df)

summary(lin.mod0)

## Create scatter plot
ggplot(house.df, aes(x = PropertySqFt, y = Price)) +
  geom_point() +
  stat_smooth(method = "lm")

## Create residual plot
ggplot(lin.mod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model 1)', x='Fitted Values', y='Residuals')

### Model 1: Log-transformed Price
house.df$LogPrice <- log10(house.df$Price)

lin.mod1 <- lm(LogPrice ~ PropertySqFt + Beds + Bath, house.df)

summary(lin.mod1)

## Create scatter plot
ggplot(house.df, aes(x = Bath, y = LogPrice)) +
  geom_point() +
  stat_smooth(method = "lm")

## Create residual plot
ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model 2)', x='Fitted Values', y='Residuals')

### Model 2: Reduced predictors
lin.mod2 <- lm(Price ~ PropertySqFt + Bath, house.df)

summary(lin.mod2)

## Creat scatter plot
ggplot(house.df, aes(x = PropertySqFt, y = Price)) +
  geom_point() +
  stat_smooth(method = "lm")

## Create residual plot
ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model 3)', x='Fitted Values', y='Residuals')

##################
# End of Lab 2
##################
