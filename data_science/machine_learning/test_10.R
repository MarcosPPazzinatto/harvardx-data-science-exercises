# 1. Preparação -----------------------------------------------------------
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)

data(brca)

# Escalar todas as 30 variáveis numéricas
x_scaled <- scale(brca$x)

# 2. Particionamento (80 % treino / 20 % teste) ---------------------------
set.seed(1)                          # <- seed pedida para a partição
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)

train_x <- x_scaled[-test_index, ]
test_x  <- x_scaled[ test_index, ]
train_y <- brca$y[-test_index]
test_y  <- brca$y[ test_index]

# 3. Treino do kNN --------------------------------------------------------
set.seed(7)                          # <- seed pedida para o modelo
ctrl  <- trainControl(method = "cv", number = 10)
grid  <- data.frame(k = seq(3, 21, 2))   # usa apenas valores ímpares de 3 a 21

knn_fit <- train(
  x = train_x,
  y = train_y,
  method     = "knn",
  tuneGrid   = grid,
  trControl  = ctrl
)

knn_fit$bestTune       # <-- valor final de k




# 4. Avaliação ------------------------------------------------------------
pred_knn <- predict(knn_fit, test_x)
acc      <- mean(pred_knn == test_y)
acc





