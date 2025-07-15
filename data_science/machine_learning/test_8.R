
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)



x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]



# set.seed(7)
set.seed(7, sample.kind = "Rounding") # simulate R 3.5


predict_kmeans <- function(x, k) {
  centers <- k$centers # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances)) # select cluster with min distance to center
}



tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn",
                   tuneGrid = tuning)
train_knn$bestTune



set.seed(5, sample.kind = "Rounding") # simulate R 3.5

train_loess <- train(train_x, train_y, method = "gamLoess")

mean(loess_preds == test_y)


















