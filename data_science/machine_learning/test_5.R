# Limpar ambiente
rm(list = ls())
options(digits = 3)

# Bibliotecas
library(dslabs)
library(caret)
library(tidyverse)
library(gam)

# Dados
data(brca)

# Escalar os dados
x_scaled <- scale(brca$x)
y <- brca$y

# Dividir treino/teste usando dados escalados
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index, ]
test_y <- y[test_index]
train_x <- x_scaled[-test_index, ]
train_y <- y[-test_index]

# Treinar modelo Loess (gamLoess)
set.seed(5)
fit_loess <- train(train_x, train_y, method = "gamLoess")

# Prever
y_hat <- predict(fit_loess, test_x)

# Acurácia
mean(y_hat == test_y)
