######################
##### Lab 3 code #####
######################

library(readr)
library(ggplot2)
library(class)
library(caret)
library(dplyr)

####################################
##### Abalone Data Preparation #####
####################################

# read dataset
abalone.data <- read.csv("C:/Users/mlitc/Documents/Data_Analytics/lab3/abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings
abalone.data$age.group <- cut(abalone.data$rings,br = c(0, 8, 11, 35), labels = c("young", "adult", "old"))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"

abalone.data$age.group <- factor(abalone.data$age.group,levels = c("young", "adult", "old"))

#######################
##### Exercise 1  #####
#######################

set.seed(42)

n <- nrow(abalone.data)
train.index <- sample(1:n, size = 0.7 * n)

train.data <- abalone.data[train.index, ]
test.data  <- abalone.data[-train.index, ]

k.start <- round(sqrt(n))

#### Model 1: first subset of features (columns 2:4)
knn.pred.1 <- knn(train = train.data[, 2:4],
                  test  = test.data[, 2:4],
                  cl    = train.data$age.group,
                  k     = k.start)

table.1 <- table(predicted = knn.pred.1, actual = test.data$age.group)
cat("\nModel 1 Contingency Table\n")
print(table.1)

acc.1 <- sum(diag(table.1)) / length(test.data$age.group)
cat("Model 1 Accuracy =", round(acc.1, 4), "\n")

#### Model 2: second subset of features (columns 5:8)
knn.pred.2 <- knn(train = train.data[, 5:8],
                  test  = test.data[, 5:8],
                  cl    = train.data$age.group,
                  k     = k.start)

table.2 <- table(predicted = knn.pred.2, actual = test.data$age.group)
cat("\nModel 2 Contingency Table\n")
print(table.2)

acc.2 <- sum(diag(table.2)) / length(test.data$age.group)
cat("Model 2 Accuracy =", round(acc.2, 4), "\n")

#### determines the better performing model and prints that
if (acc.1 >= acc.2) {
  best.features <- 2:4
  best.model <- "Model 1"
  best.acc <- acc.1
} else {
  best.features <- 5:8
  best.model <- "Model 2"
  best.acc <- acc.2
}

cat("\nBetter performing model = ", best.model,
    " with accuracy =", round(best.acc, 4), "\n")

##### Find Optimal k for Best Feature Set

k.values <- seq(5, 105, by = 10)
accuracy.values <- numeric(length(k.values))

for (i in seq_along(k.values)) {
  k <- k.values[i]
  pred <- knn(train = train.data[, best.features],
              test  = test.data[, best.features],
              cl    = train.data$age.group,
              k     = k)
  cm <- table(pred, test.data$age.group)
  accuracy.values[i] <- sum(diag(cm)) / length(test.data$age.group)
}

plot(k.values, accuracy.values, type = "b", pch = 19, col = "steelblue",
     main = paste("Accuracy vs k (", best.model, " features )"),
     xlab = "k (neighbors)", ylab = "Classification Accuracy")

best.k <- k.values[which.max(accuracy.values)]
cat("\nOptimal k =", best.k,
    " with accuracy =", round(max(accuracy.values), 4), "\n")

######################
##### Exercise 2 #####
######################

cluster.data <- scale(abalone.data[, best.features])

pairs(cluster.data, main = "Selected Features (Scaled)")

############################
#### K-Means Clustering ####
############################

k.list <- 2:6
wcss <- numeric(length(k.list))
sil.mean.km <- numeric(length(k.list))

for (i in seq_along(k.list)) {
  k <- k.list[i]
  km <- kmeans(cluster.data, centers = k, nstart = 25)
  wcss[i] <- km$tot.withinss
  sil <- silhouette(km$cluster, dist(cluster.data))
  sil.mean.km[i] <- mean(sil[, 3])
}

# plot WCSS and Silhouette
par(mfrow = c(1, 2))
plot(k.list, wcss, type = "b", pch = 19, col = "darkorange",
     main = "K-Means Elbow (WCSS)",
     xlab = "K", ylab = "Within-Cluster SS")
plot(k.list, sil.mean.km, type = "b", pch = 19, col = "forestgreen",
     main = "K-Means Average Silhouette",
     xlab = "K", ylab = "Avg Silhouette Width")

best.k.kmeans <- k.list[which.max(sil.mean.km)]
cat("\nBest K for K-Means =", best.k.kmeans, "\n")

final.kmeans <- kmeans(cluster.data, centers = best.k.kmeans, nstart = 25)
fviz_cluster(final.kmeans, data = cluster.data,
             main = paste("K-Means ( K =", best.k.kmeans, " )"))

# silhouette plot
sil.kmeans <- silhouette(final.kmeans$cluster, dist(cluster.data))
fviz_silhouette(sil.kmeans)

#### PAM Clustering

sil.mean.pam <- numeric(length(k.list))
sum.diss <- numeric(length(k.list))

for (i in seq_along(k.list)) {
  k <- k.list[i]
  pam.model <- pam(cluster.data, k)
  sum.diss[i] <- pam.model$objective[1]
  sil <- silhouette(pam.model$clustering, dist(cluster.data))
  sil.mean.pam[i] <- mean(sil[, 3])
}

# plot dissimilarity and silhouette
par(mfrow = c(1, 2))
plot(k.list, sum.diss, type = "b", pch = 19, col = "tomato",
     main = "PAM – Sum of Dissimilarities",
     xlab = "K", ylab = "Sum Dissimilarity")
plot(k.list, sil.mean.pam, type = "b", pch = 19, col = "royalblue",
     main = "PAM – Average Silhouette Width",
     xlab = "K", ylab = "Avg Silhouette Width")

best.k.pam <- k.list[which.max(sil.mean.pam)]
cat("\nBest K for PAM =", best.k.pam, "\n")

final.pam <- pam(cluster.data, best.k.pam)
fviz_cluster(final.pam, geom = "point", ellipse.type = "convex",
             main = paste("PAM ( K =", best.k.pam, " )"))

sil.pam <- silhouette(final.pam$clustering, dist(cluster.data))
fviz_silhouette(sil.pam)

