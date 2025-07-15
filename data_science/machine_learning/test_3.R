# Limpar o ambiente e definir precisão
rm(list = ls())
options(digits = 3)

# Carregar bibliotecas necessárias
library(dslabs)
library(caret)
library(tidyverse)
library(randomForest)

# Carregar os dados
data(brca)
x <- brca$x
y <- brca$y

# Dividir treino e teste
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
x_train <- x[-test_index, ]
y_train <- y[-test_index]
x_test <- x[test_index, ]
y_test <- y[test_index]

# Treinar modelo Random Forest
set.seed(9)
fit_rf <- train(
  x_train, y_train,
  method = "rf",
  tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
  importance = TRUE
)

# Melhor valor de mtry
best_mtry <- fit_rf$bestTune$mtry
print(paste("Melhor mtry:", best_mtry))

# Previsão no conjunto de teste
y_hat_rf <- predict(fit_rf, x_test)

# Calcular acurácia
accuracy_rf <- mean(y_hat_rf == y_test)
print(paste("Acurácia:", accuracy_rf))

# Ver variáveis mais importantes
importance <- varImp(fit_rf)$importance
print(importance)  # Opcional: para ver todas

# Ordenar pela importância e extrair a variável mais importante
importance_sorted <- importance[order(importance[,1], decreasing = TRUE), , drop = FALSE]
most_important_variable <- rownames(importance_sorted)[1]
print(paste("Variável mais importante:", most_important_variable))





fit_rf <- train(
  x_train, y_train,
  method = "rf",
  tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
  importance = TRUE
)


# Previsões no conjunto de teste
y_hat_rf <- predict(fit_rf, x_test)

# Calcular a acurácia com precisão alta
accuracy_rf <- mean(y_hat_rf == y_test)
print(accuracy_rf)




# Limpar ambiente
rm(list = ls())
options(digits = 3)

# Bibliotecas
library(dslabs)
library(caret)
library(tidyverse)
library(randomForest)

# Dados
data(brca)
x <- brca$x
y <- brca$y

# Separar treino/teste
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
x_train <- x[-test_index, ]
y_train <- y[-test_index]
x_test <- x[test_index, ]
y_test <- y[test_index]

# Controle de treino com 10-fold CV
control <- trainControl(method = "cv", number = 10)

# Treinar modelo com controle e importance
set.seed(9)
fit_rf <- train(
  x_train, y_train,
  method = "rf",
  tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
  importance = TRUE,
  trControl = control
)

# Melhor mtry
fit_rf$bestTune

# Prever
y_hat_rf <- predict(fit_rf, x_test)

# Acurácia
accuracy_rf <- mean(y_hat_rf == y_test)
print(accuracy_rf)




