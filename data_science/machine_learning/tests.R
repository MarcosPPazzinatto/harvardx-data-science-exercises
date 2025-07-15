library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


# Tabela com contagem por tipo e sexo
table(x, y)

# Proporção de mulheres por grupo
prop.table(table(x, y), margin = 1)


table(a, b)

library(caret)
library(caret)
library(e1071)  # carrega dependências necessárias do caret


table(y_hat, y)


# Previsão
y_hat <- ifelse(x == "inclass", "Female", "Male")
y_hat <- factor(y_hat, levels = levels(y))

# Calcular sensibilidade
sensitivity(y_hat, y)


specificity(y_hat, y)


library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)


set.seed(76)
# line of code
test <- iris[test_index,]
train <- iris[-test_index,]


train <- iris[-test_index, ]
test <- iris[test_index, ]

cutoffs <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)

accuracy <- function(pred, true) {
  mean(pred == true)
}

best_cutoff <- cutoffs[which.max(sapply(cutoffs, function(cutoff) {
  y_hat <- ifelse(train$Petal.Width > cutoff, "virginica", "versicolor")
  accuracy(y_hat, train$Species)
}))]




y_hat_test <- ifelse(test$Petal.Width > best_cutoff, "virginica", "versicolor")
accuracy_test <- mean(y_hat_test == test$Species)


accuracy_test


# Passo 1: definir os conjuntos
train <- iris[-test_index, ]
test <- iris[test_index, ]

# Função de acurácia
accuracy <- function(pred, true) {
  mean(pred == true)
}

# Testar todos os pares de cutoffs
length_range <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
width_range  <- seq(min(train$Petal.Width),  max(train$Petal.Width),  0.1)

best_acc <- 0
best_cutoff_length <- 0
best_cutoff_width <- 0

for (len_cutoff in length_range) {
  for (wid_cutoff in width_range) {
    y_hat <- ifelse(train$Petal.Length > len_cutoff & train$Petal.Width > wid_cutoff,
                    "virginica", "versicolor")
    acc <- accuracy(y_hat, train$Species)
    if (acc > best_acc) {
      best_acc <- acc
      best_cutoff_length <- len_cutoff
      best_cutoff_width <- wid_cutoff
    }
  }
}

# Agora usar no conjunto de teste
y_hat_test <- ifelse(test$Petal.Length > best_cutoff_length & test$Petal.Width > best_cutoff_width,
                     "virginica", "versicolor")

accuracy_test <- mean(y_hat_test == test$Species)
accuracy_test



set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98, 0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15,0.85))



mean(test == 1)


# Probabilidade de ter a doença dado que o teste foi negativo
mean(disease[test == 0] == 1)


mean(disease[test == 1] == 1)


library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

library(tidyverse)
library(caret)
library(MASS)

set.seed(1)
n <- 100
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set  <- dat[test_index, ]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))  # RMSE
})

mean_rmse <- mean(rmse)
sd_rmse <- sd(rmse)

mean_rmse
sd_rmse


ns <- c(100, 500, 1000, 5000, 10000)


library(tidyverse)
library(caret)
library(MASS)

# Matriz de covariância e média
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

# Função de simulação
rmse_sim <- function(n) {
  dat <- MASS::mvrnorm(n = n, mu = c(69, 69), Sigma = Sigma) %>%
    data.frame() %>%
    setNames(c("x", "y"))
  
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
    train_set <- dat[-test_index, ]
    test_set  <- dat[test_index, ]
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  
  c(mean = mean(rmse), sd = sd(rmse))
}

# Rodar para todos os tamanhos
set.seed(1)
results <- sapply(c(100, 500, 1000, 5000, 10000), rmse_sim)
round(results, 3)


set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


Sigma <- 9 * matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)


library(tidyverse)
library(caret)
library(MASS)

set.seed(1)
n <- 100
Sigma <- 9 * matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set  <- dat[test_index, ]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rmse)
sd(rmse)


library(tidyverse)
library(caret)
library(MASS)

set.seed(1)

# Criar dados
Sigma <- matrix(c(1.0, 0.75, 0.75,
                  0.75, 1.0, 0.25,
                  0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, mu = c(0, 0, 0), Sigma = Sigma) %>%
  data.frame() %>%
  setNames(c("y", "x_1", "x_2"))

# Dividir em treino e teste
set.seed(1)
test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
train_set <- dat[-test_index, ]
test_set <- dat[test_index, ]

# Modelos
fit_x1 <- lm(y ~ x_1, data = train_set)
fit_x2 <- lm(y ~ x_2, data = train_set)
fit_both <- lm(y ~ x_1 + x_2, data = train_set)

# RMSEs
rmse <- function(model) {
  pred <- predict(model, newdata = test_set)
  sqrt(mean((pred - test_set$y)^2))
}

c(
  x1 = rmse(fit_x1),
  x2 = rmse(fit_x2),
  both = rmse(fit_both)
)

# Ajuste LOESS com y convertido para numérico
fit <- loess(as.numeric(y) ~ x_2, degree = 1, span = 0.5, data = mnist_27$train)

# Previsão
y_hat <- predict(fit, newdata = mnist_27$test)

# Classificação binária: 7 se > 0.5
y_pred <- ifelse(y_hat > 0.5, 7, 2)

# Calcular acurácia
mean(y_pred == mnist_27$test$y, na.rm = TRUE)


mean(y_pred == mnist_27$test$y, na.rm = TRUE)
# [1] 0.5175879



library(dslabs)
library(caret)
library(dplyr)

set.seed(1)

# Dados
data("heights")
y <- heights$sex
x <- heights$height

# Dividir treino/teste
set.seed(1)
train_index <- createDataPartition(y, p = 0.5, list = FALSE)
train_set <- data.frame(x = x[train_index], y = y[train_index])
test_set <- data.frame(x = x[-train_index], y = y[-train_index])

# Testar vários valores de k
k_vals <- seq(1, 101, 3)

F_1_scores <- sapply(k_vals, function(k) {
  fit <- knn3(y ~ x, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = test_set$y)
})

# Resultado
max_F1 <- max(F_1_scores)
best_k <- k_vals[which.max(F_1_scores)]

max_F1
best_k


best_k <- min(k_vals[which(F_1_scores == max_F1)])
best_k

round(F_1_scores, 4)
F_1_scores[F_1_scores == max_F1]

which(F_1_scores == max_F1)


k_vals[which(F_1_scores == max_F1)]
min(k_vals[which(F_1_scores == max_F1)])


min(k_vals[which(F_1_scores == max_F1)])



# Carregar pacotes
library(dslabs)
library(caret)
library(dplyr)

# Dados
data("heights")
y <- heights$sex
x <- heights$height

# Dividir treino/teste
set.seed(1)
train_index <- createDataPartition(y, p = 0.5, list = FALSE)
train_set <- data.frame(x = x[train_index], y = y[train_index])
test_set  <- data.frame(x = x[-train_index], y = y[-train_index])

# Valores de k a testar
k_vals <- seq(1, 101, 3)

# Calcular F1 para cada k
F_1_scores <- sapply(k_vals, function(k) {
  fit <- knn3(y ~ x, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = test_set$y)
})

# Resultado
max_F1 <- max(F_1_scores)
best_k <- min(k_vals[which(F_1_scores == max_F1)])

# Mostrar resultado
max_F1
best_k




library(dslabs)
library(class)
library(caret)
library(dplyr)
library(purrr)

# Set seed
set.seed(1)

# Particionar os dados
data(heights)
y <- heights$sex
x <- heights$height
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- heights[-test_index, ]
test_set  <- heights[ test_index, ]

# Avaliar diferentes valores de k
ks <- seq(1, 101, 3)
f1_scores <- sapply(ks, function(k){
  fit <- knn(train = as.matrix(train_set$height),
             test  = as.matrix(test_set$height),
             cl    = train_set$sex,
             k     = k)
  F_meas(data = factor(fit), reference = factor(test_set$sex))
})

# Mostrar os resultados
data.frame(k = ks, F1 = f1_scores) %>%
  filter(F1 == max(F1))


library(dslabs)
library(tidyverse)
library(caret)
data("heights")

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]


library(dslabs)
data("tissue_gene_expression")


library(dslabs)
library(class)
library(caret)

# Carregar os dados
data("tissue_gene_expression")

# Separar as variáveis
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# Dividir em treino e teste
set.seed(1)
train_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
x_train <- x[train_index, ]
x_test  <- x[-train_index, ]
y_train <- y[train_index]
y_test  <- y[-train_index]

# Avaliar diferentes valores de k
ks <- c(1, 3, 5, 7, 9, 11)
accuracies <- sapply(ks, function(k) {
  y_pred <- knn(train = x_train, test = x_test, cl = y_train, k = k)
  mean(y_pred == y_test)
})

# Mostrar resultados
data.frame(k = ks, Accuracy = accuracies)







library(dslabs)
library(class)
library(caret)

data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

set.seed(1)
index <- createDataPartition(y, p = 0.5, list = FALSE)
train_x <- x[index, ]
test_x <- x[-index, ]
train_y <- y[index]
test_y <- y[-index]

ks <- c(1,3,5,7,9,11)
acc <- sapply(ks, function(k){
  y_hat <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  mean(y_hat == test_y)
})
acc


round(accuracies, 5)




library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)



library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# Primeiro índice de reamostragem
first_index <- indexes[[1]]

# Contar quantas vezes aparecem os índices 3, 4 e 7
sum(first_index == 3)
sum(first_index == 4)
sum(first_index == 7)



library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# Contar quantas vezes o número 3 aparece em todas as amostragens
total_3 <- sum(sapply(indexes, function(ind) sum(ind == 3)))
total_3



set.seed(1)

# Rodar 10.000 simulações do quantil 75%
q75_vals <- replicate(10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

# Calcular média e erro padrão
expected_value <- mean(q75_vals)
standard_error <- sd(q75_vals)

# Mostrar resultados
expected_value
standard_error


set.seed(1)
y <- rnorm(100, 0, 1)



library(caret)

# Passo 1: gerar y
set.seed(1)
y <- rnorm(100, 0, 1)

# Passo 2: gerar índices bootstrap
set.seed(1)
indexes <- createResample(y, 10000)

# Passo 3: calcular quantis 75% para cada reamostragem
q75_bootstrap <- sapply(indexes, function(i) {
  quantile(y[i], 0.75)
})

# Passo 4: calcular média e erro padrão
expected_value <- mean(q75_bootstrap)
standard_error <- sd(q75_bootstrap)

# Mostrar resultados
expected_value
standard_error


install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1



library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]



pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[, i][y == 0], x[, i][y == 1], var.equal = TRUE)$p.value
}



ind <- which(pvals < 0.01)
length(ind)



library(tidyverse)
library(caret)

# Dados simulados
set.seed(1996)
n <- 10000
p <- 1000
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

# p-valor de cada preditor (t-test)
pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[, i][y == 0], x[, i][y == 1], var.equal = TRUE)$p.value
}

# Selecionar colunas com p < 0.01
ind <- which(pvals < 0.01)
x_subset <- x[, ind]

# Repetir validação cruzada
set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results$Accuracy





# Pacotes necessários
library(tidyverse)
library(caret)

library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# Etapa 2: Calcular o valor-p com t-test para cada coluna de x
pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[, i][y == 0], x[, i][y == 1], var.equal = TRUE)$p.value
}

# Etapa 3: Selecionar os índices com p < 0.01
ind <- which(pvals < 0.01)

# Etapa 4: Criar o x_subset com as colunas significativas
x_subset <- x[, ind]

# Etapa 5: Treinar modelo com validação cruzada
set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results$Accuracy



library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)


library(dslabs)
library(caret)
library(rpart)

set.seed(1991)

data("tissue_gene_expression")

# Definir grade de cp
cp_values <- seq(0, 0.1, 0.01)

# Treinar modelo com cross-validation e grid de cp
fit <- train(
  y ~ .,
  data = tissue_gene_expression,
  method = "rpart",
  tuneGrid = data.frame(cp = cp_values)
)

# Visualizar resultados para cp vs accuracy
plot(fit)

# Encontrar cp com maior accuracy
best_cp <- fit$bestTune$cp
best_cp



library(dslabs)
library(caret)
library(rpart)

set.seed(1991)
data("tissue_gene_expression")

# Renomear as colunas para algo simples
colnames(tissue_gene_expression)[-ncol(tissue_gene_expression)] <- paste0("x", 1:(ncol(tissue_gene_expression)-1))

cp_values <- seq(0, 0.1, 0.01)

fit <- train(
  y ~ .,
  data = tissue_gene_expression,
  method = "rpart",
  tuneGrid = data.frame(cp = cp_values)
)

plot(fit)
fit$bestTune$cp



class(tissue_gene_expression)

colnames(tissue_gene_expression$x) <- paste0("x", 1:ncol(tissue_gene_expression$x))


# Criar um data frame combinando x e y
dat <- data.frame(tissue_gene_expression$x)
dat$y <- tissue_gene_expression$y

cp_values <- seq(0, 0.1, 0.01)

fit <- train(
  y ~ .,
  data = dat,
  method = "rpart",
  tuneGrid = data.frame(cp = cp_values),
  trControl = trainControl(method = "cv")
)

plot(fit)
fit$bestTune$cp



# Ver tabela completa de resultados
fit$results

# Identificar o valor de cp com maior Accuracy
best_row <- fit$results[which.max(fit$results$Accuracy), ]
best_cp <- best_row$cp
best_cp



library(dslabs)
library(caret)
library(rpart)

set.seed(1991)
data("tissue_gene_expression")

# Renomear as colunas de x para nomes simples
colnames(tissue_gene_expression$x) <- paste0("x", 1:ncol(tissue_gene_expression$x))

# Criar data frame combinando x e y
dat <- data.frame(tissue_gene_expression$x)
dat$y <- tissue_gene_expression$y

cp_values <- seq(0, 0.1, 0.01)

fit2 <- train(
  y ~ .,
  data = dat,
  method = "rpart",
  tuneGrid = data.frame(cp = cp_values),
  trControl = trainControl(method = "cv"),
  control = rpart.control(minsplit = 0)
)

plot(fit2)
fit2$results[fit2$results$cp == fit2$bestTune$cp, "Accuracy"]


fit2$results[fit2$results$Accuracy == max(fit2$results$Accuracy), "Accuracy"]



print(fit2$results)



library(dslabs)
library(caret)
library(rpart)

set.seed(1991)
data("tissue_gene_expression")

colnames(tissue_gene_expression$x) <- paste0("x", 1:ncol(tissue_gene_expression$x))
dat <- data.frame(tissue_gene_expression$x)
dat$y <- tissue_gene_expression$y

cp_values <- seq(0, 0.1, 0.01)

fit2 <- train(
  y ~ .,
  data = dat,
  method = "rpart",
  tuneGrid = data.frame(cp = cp_values),
  trControl = trainControl(method = "cv"),
  control = rpart.control(minsplit = 0)
)

# Matriz de confusão do melhor modelo
cm <- confusionMatrix(fit2)
print(cm)

# Acurácia do melhor modelo
acc <- fit2$results[fit2$results$cp == fit2$bestTune$cp, "Accuracy"]
print(acc)





library(dslabs)
library(caret)
library(rpart)

set.seed(1991)
data("tissue_gene_expression")

# Renomear colunas para evitar erros
colnames(tissue_gene_expression$x) <- paste0("x", 1:ncol(tissue_gene_expression$x))

# Criar data frame combinado
dat <- data.frame(tissue_gene_expression$x)
dat$y <- tissue_gene_expression$y

# Criar folds para cross-validation
folds <- createFolds(dat$y, k = 10, list = TRUE)

accuracies <- numeric(length(folds))

# Loop para validação cruzada manual
for (i in seq_along(folds)) {
  train_index <- setdiff(seq_len(nrow(dat)), folds[[i]])
  test_index <- folds[[i]]
  
  train_data <- dat[train_index, ]
  test_data <- dat[test_index, ]
  
  # Treinar modelo rpart com minsplit=0
  fit <- rpart(y ~ ., data = train_data, control = rpart.control(minsplit = 0), cp = 0)
  
  # Prever no conjunto de teste
  preds <- predict(fit, newdata = test_data, type = "class")
  
  # Calcular acurácia
  accuracies[i] <- mean(preds == test_data$y)
}

# Média da acurácia nos 10 folds
mean_accuracy <- mean(accuracies)
print(mean_accuracy)


install.packages("rpart.plot")


library(dslabs)
library(caret)
library(rpart)
library(rpart.plot)

set.seed(1991)
data("tissue_gene_expression")

cp_values <- seq(0, 0.1, 0.01)

fit2 <- train(
  y ~ .,
  data = data.frame(tissue_gene_expression$x, y = tissue_gene_expression$y),
  method = "rpart",
  tuneGrid = data.frame(cp = cp_values),
  trControl = trainControl(method = "cv"),
  control = rpart.control(minsplit = 0)
)

rpart.plot(fit2$finalModel)




library(dslabs)
library(caret)
library(randomForest)

set.seed(1991)
data("tissue_gene_expression")

# Preparar o dataset
dat <- data.frame(tissue_gene_expression$x)
dat$y <- tissue_gene_expression$y

# Valores para testar de mtry
mtry_values <- seq(50, 200, 25)

# Criar grid para treino
tunegrid <- expand.grid(mtry = mtry_values)

# Treinar o modelo Random Forest com cross-validation
fit <- train(
  y ~ .,
  data = dat,
  method = "rf",
  tuneGrid = tunegrid,
  trControl = trainControl(method = "cv"),
  nodesize = 1
)

# Mostrar resultados
print(fit)
fit$bestTune$mtry


install.packages("titanic")

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


library(titanic)
library(caret)
library(tidyverse)
library(rpart)

# Limpeza dos dados, conforme enunciado (se já não tiver feito)
titanic_clean <- titanic_train %>%
  mutate(
    Survived = factor(Survived),
    Embarked = factor(Embarked),
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    FamilySize = SibSp + Parch + 1
  ) %>%
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Definir semente
set.seed(42)

# Criar partições
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)

train_set <- titanic_clean[-test_index, ]
test_set <- titanic_clean[test_index, ]

# Quantidade de observações no treino e teste
n_train <- nrow(train_set)
n_test <- nrow(test_set)

# Proporção de sobreviventes no treino
prop_survived <- mean(train_set$Survived == "1")

print(n_train)
print(n_test)
print(prop_survived)



set.seed(3)

# Fazer chute aleatório para o conjunto de teste
guess <- sample(c(0,1), size = nrow(test_set), replace = TRUE)

# Calcular acurácia do chute (comparar com os valores reais)
accuracy_guess <- mean(as.numeric(as.character(test_set$Survived)) == guess)

print(accuracy_guess)




# Proporção de sobreviventes mulheres no treino
prop_female_survived <- mean(train_set$Survived[train_set$Sex == "female"] == "1")

# Proporção de sobreviventes homens no treino
prop_male_survived <- mean(train_set$Survived[train_set$Sex == "male"] == "1")

print(prop_female_survived)
print(prop_male_survived)



# Primeiro, calcular a taxa de sobrevivência para cada sexo no conjunto de treino
prop_female_survived <- mean(train_set$Survived[train_set$Sex == "female"] == "1")
prop_male_survived <- mean(train_set$Survived[train_set$Sex == "male"] == "1")

# Fazer predição para o conjunto de teste baseado nessas taxas
pred_sex <- ifelse(
  test_set$Sex == "female",
  ifelse(prop_female_survived > 0.5, "1", "0"),
  ifelse(prop_male_survived > 0.5, "1", "0")
)

# Calcular acurácia comparando predição com o valor real
accuracy_sex <- mean(pred_sex == test_set$Survived)

print(accuracy_sex)



# Calcular proporção de sobreviventes por classe no treino
survival_by_class <- train_set %>%
  group_by(Pclass) %>%
  summarise(prop_survived = mean(Survived == "1"))

print(survival_by_class)




# Obter taxa de sobrevivência por classe no treino
survival_by_class <- train_set %>%
  group_by(Pclass) %>%
  summarise(prop_survived = mean(Survived == "1"))

# Criar uma função de predição baseada na taxa da classe
predict_survival_by_class <- function(pclass) {
  if (survival_by_class$prop_survived[survival_by_class$Pclass == pclass] > 0.5) {
    return("1")  # Sobrevive
  } else {
    return("0")  # Não sobrevive
  }
}

# Aplicar a predição no conjunto de teste
test_set$predicted_survival <- sapply(test_set$Pclass, predict_survival_by_class)

# Calcular a acurácia
accuracy <- mean(test_set$predicted_survival == test_set$Survived)
print(accuracy)



# Calcular proporção de sobreviventes por sexo e classe
survival_by_sex_class <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarise(prop_survived = mean(Survived == "1"))

# Mostrar grupos com mais de 50% de sobreviventes
survival_by_sex_class %>% 
  filter(prop_survived > 0.5)





# Calcular proporção de sobreviventes por sexo e classe no conjunto de treino
survival_by_sex_class <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarise(prop_survived = mean(Survived == "1"), .groups = "drop")

# Juntar as proporções no conjunto de teste (por sexo e classe)
test_set <- test_set %>%
  left_join(survival_by_sex_class, by = c("Sex", "Pclass"))

# Fazer a predição: se prop_survived > 0.5, prever "1", senão "0"
test_set <- test_set %>%
  mutate(predicted_survival = ifelse(prop_survived > 0.5, "1", "0"))

# Calcular a acurácia da predição
accuracy <- mean(test_set$predicted_survival == test_set$Survived)
print(accuracy)


# Extrair os valores de balanced accuracy dos três modelos
balanced_accuracies <- c(
  sex_only = cm_sex$byClass["Balanced Accuracy"],
  class_only = cm_class$byClass["Balanced Accuracy"],
  sex_class_combined = cm_sex_class$byClass["Balanced Accuracy"]
)

# Mostrar o maior valor
max_balanced_accuracy <- max(balanced_accuracies)
print(max_balanced_accuracy)




library(caret)

# Supondo que você tenha os dados de teste: test_set

# Criar predições para cada modelo (exemplo para o modelo sex_only)
pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"))  # só exemplo, ajusta conforme seu modelo
cm_sex <- confusionMatrix(pred_sex, test_set$Survived, positive = "1")

# Para class_only
# pred_class <- ... (sua predição para o modelo class_only)
cm_class <- confusionMatrix(pred_class, test_set$Survived, positive = "1")

# Para sex_class_combined
# pred_sex_class <- ... (sua predição para o modelo combinado)
cm_sex_class <- confusionMatrix(pred_sex_class, test_set$Survived, positive = "1")




# Previsão para class_only
test_set <- test_set %>%
  left_join(survival_by_class, by = "Pclass") %>%
  mutate(predicted_survival_class = ifelse(prop_survived > 0.5, "1", "0")) 

pred_class <- factor(test_set$predicted_survival_class)

cm_class <- confusionMatrix(pred_class, test_set$Survived, positive = "1")



# Previsão para sex_class_combined
test_set <- test_set %>%
  left_join(survival_by_sex_class, by = c("Sex", "Pclass")) %>%
  mutate(predicted_survival_sex_class = ifelse(prop_survived > 0.5, "1", "0"))

pred_sex_class <- factor(test_set$predicted_survival_sex_class)

cm_sex_class <- confusionMatrix(pred_sex_class, test_set$Survived, positive = "1")




max_balanced_accuracy <- max(c(
  cm_sex$byClass["Balanced Accuracy"],
  cm_class$byClass["Balanced Accuracy"],
  cm_sex_class$byClass["Balanced Accuracy"]
))

print(max_balanced_accuracy)



library(caret)

# Transformar previsões em fatores, se ainda não estiverem
pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"))
pred_class <- factor(ifelse(test_set$predicted_survival_class == "1", "1", "0"))
pred_sex_class <- factor(ifelse(test_set$predicted_survival_sex_class == "1", "1", "0"))

# Calcular F1 score para cada modelo
f1_sex <- F_meas(pred_sex, test_set$Survived, relevant = "1")
f1_class <- F_meas(pred_class, test_set$Survived, relevant = "1")
f1_sex_class <- F_meas(pred_sex_class, test_set$Survived, relevant = "1")

# Imprimir resultados
print(c(sex_only = f1_sex, class_only = f1_class, sex_class_combined = f1_sex_class))

# Valor máximo de F1 score
max_f1 <- max(f1_sex, f1_class, f1_sex_class)
print(max_f1)




# Ajustar levels para garantir consistência
test_set$Survived <- factor(test_set$Survived, levels = c("0", "1"))

# Transformar previsões em fatores com mesmos levels
pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"), levels = c("0", "1"))
pred_class <- factor(ifelse(test_set$predicted_survival_class == "1", "1", "0"), levels = c("0", "1"))
pred_sex_class <- factor(ifelse(test_set$predicted_survival_sex_class == "1", "1", "0"), levels = c("0", "1"))

# Calcular F1 scores
f1_sex <- F_meas(pred_sex, test_set$Survived, relevant = "1")
f1_class <- F_meas(pred_class, test_set$Survived, relevant = "1")
f1_sex_class <- F_meas(pred_sex_class, test_set$Survived, relevant = "1")

# Imprimir resultados
print(c(sex_only = f1_sex, class_only = f1_class, sex_class_combined = f1_sex_class))

# Máximo F1 score
max_f1 <- max(f1_sex, f1_class, f1_sex_class)
print(max_f1)


# Ajustar levels para garantir consistência
test_set$Survived <- factor(test_set$Survived, levels = c("0", "1"))

# Transformar previsões em fatores com levels iguais
pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"), levels = c("0", "1"))
pred_class <- factor(ifelse(test_set$predicted_survival_class == "1", "1", "0"), levels = c("0", "1"))
pred_sex_class <- factor(ifelse(test_set$predicted_survival_sex_class == "1", "1", "0"), levels = c("0", "1"))

# Calcular F1 scores
f1_sex <- F_meas(pred_sex, test_set$Survived, relevant = "1")
f1_class <- F_meas(pred_class, test_set$Survived, relevant = "1")
f1_sex_class <- F_meas(pred_sex_class, test_set$Survived, relevant = "1")

# Imprimir resultados
print(c(sex_only = f1_sex, class_only = f1_class, sex_class_combined = f1_sex_class))

# Valor máximo de F1
max_f1 <- max(c(f1_sex, f1_class, f1_sex_class), na.rm = TRUE)
print(max_f1)




library(caret)

# Ajustar níveis para garantir consistência
test_set$Survived <- factor(test_set$Survived, levels = c("0", "1"))

pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"), levels = c("0", "1"))
pred_class <- factor(ifelse(test_set$predicted_survival_class == "1", "1", "0"), levels = c("0", "1"))
pred_sex_class <- factor(ifelse(test_set$predicted_survival_sex_class == "1", "1", "0"), levels = c("0", "1"))

# Calcular F1 score para cada modelo
f1_sex <- F_meas(pred_sex, test_set$Survived, relevant = "1")
f1_class <- F_meas(pred_class, test_set$Survived, relevant = "1")
f1_sex_class <- F_meas(pred_sex_class, test_set$Survived, relevant = "1")

# Imprimir resultados
print(c(sex_only = f1_sex, class_only = f1_class, sex_class_combined = f1_sex_class))

# Valor máximo de F1
max_f1 <- max(c(f1_sex, f1_class, f1_sex_class), na.rm = TRUE)
print(max_f1)



library(caret)

# Ajustar níveis para garantir consistência
test_set$Survived <- factor(test_set$Survived, levels = c("0", "1"))

pred_sex_class <- factor(ifelse(test_set$predicted_survival_sex_class == "1", "1", "0"), levels = c("0", "1"))

# Calcular F1 score para o modelo combinado
f1_sex_class <- F_meas(pred_sex_class, test_set$Survived, relevant = "1")

# Imprimir o resultado com 3 casas decimais
print(round(f1_sex_class, 3))



library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


# Extrair os valores da matriz de confusão para o modelo combinado
conf_mat <- as.matrix(cm_sex_class$table)

# Calcular precisão (precision), recall (sensibilidade) e F1 score para a classe positiva "1"
precision <- conf_mat["1","1"] / sum(conf_mat["1", ])
recall <- conf_mat["1","1"] / sum(conf_mat[, "1"])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Imprimir com mais casas decimais
print(round(f1_score, 5))





library(caret)
library(dplyr)

# Ajustar níveis para garantir consistência
test_set$Survived <- factor(test_set$Survived, levels = c("0", "1"))

# Criar a previsão para o modelo combinado sexo + classe
test_set <- test_set %>%
  left_join(survival_by_sex_class, by = c("Sex", "Pclass")) %>%
  mutate(predicted_survival_sex_class = ifelse(prop_survived > 0.5, "1", "0"))

# Transformar em fator com níveis iguais
pred_sex_class <- factor(test_set$predicted_survival_sex_class, levels = c("0", "1"))

# Gerar a matriz de confusão
cm_sex_class <- confusionMatrix(pred_sex_class, test_set$Survived, positive = "1")

# Extrair a tabela da matriz de confusão
conf_mat <- as.matrix(cm_sex_class$table)

# Calcular precisão, recall e F1 score
precision <- conf_mat["1","1"] / sum(conf_mat["1", ])
recall <- conf_mat["1","1"] / sum(conf_mat[, "1"])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Imprimir F1 score com 5 casas decimais
print(round(f1_score, 5))



library(titanic)
library(caret)
library(tidyverse)
library(rpart)

options(digits = 3)

# Preparar dados
titanic_clean <- titanic_train %>%
  mutate(
    Survived = factor(Survived),
    Embarked = factor(Embarked),
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    FamilySize = SibSp + Parch + 1
  ) %>%
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42)
train_index <- createDataPartition(titanic_clean$Survived, p = 0.8, list = FALSE)
train_set <- titanic_clean[train_index, ]
test_set <- titanic_clean[-train_index, ]

# Modelo sexo
pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"), levels = c("0", "1"))

# Modelo classe
prop_survived_by_class <- train_set %>%
  group_by(Pclass) %>%
  summarise(prop_survived = mean(Survived == "1"))

test_set <- test_set %>%
  mutate(
    Sex = as.character(Sex),
    Pclass = as.character(Pclass)
  )

prop_survived_by_sex_class <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarise(prop_survived = mean(Survived == "1")) %>%
  ungroup()

test_set <- test_set %>%
  left_join(prop_survived_by_sex_class, by = c("Sex", "Pclass")) %>%
  mutate(predicted_survival_sex_class = ifelse(prop_survived > 0.5, "1", "0"))

# Transformar a previsão em fator
pred_sex_class <- factor(test_set$predicted_survival_sex_class, levels = c("0", "1"))

# Ajustar levels para garantir consistência
test_set$Survived <- factor(test_set$Survived, levels = c("0", "1"))

# Calcular F1 score
f1_sex_class <- F_meas(pred_sex_class, test_set$Survived, relevant = "1")

print(round(f1_sex_class, 3))

test_set <- test_set %>%
  mutate(
    Sex = as.character(Sex),
    Pclass = as.integer(as.character(Pclass))  # converter para inteiro para bater com train_set
  )


test_set <- test_set %>%
  left_join(prop_survived_by_sex_class, by = c("Sex", "Pclass")) %>%
  mutate(predicted_survival_sex_class = ifelse(prop_survived > 0.5, "1", "0"))







library(caret)

set.seed(1)

# Treinar modelo Loess com método gamLoess e preditor Fare
fit_loess <- train(
  Survived ~ Fare,
  data = train_set,
  method = "gamLoess",
  trControl = trainControl(method = "cv", number = 10),
  metric = "Accuracy"
)

# Fazer previsões no conjunto de teste
pred_loess <- predict(fit_loess, newdata = test_set)

# Calcular acurácia
accuracy_loess <- mean(pred_loess == test_set$Survived)
print(accuracy_loess)




library(caret)
set.seed(1)

# Modelo 1: usando só a idade (Age)
fit_age <- train(
  Survived ~ Age,
  data = train_set,
  method = "glm",
  family = "binomial"
)

pred_age <- predict(fit_age, newdata = test_set)
accuracy_age <- mean(pred_age == test_set$Survived)
print(accuracy_age)

# Modelo 2: usando quatro preditores (Sex, Pclass, Fare, Age)
fit_4pred <- train(
  Survived ~ Sex + Pclass + Fare + Age,
  data = train_set,
  method = "glm",
  family = "binomial"
)

pred_4pred <- predict(fit_4pred, newdata = test_set)
accuracy_4pred <- mean(pred_4pred == test_set$Survived)
print(accuracy_4pred)

# Modelo 3: usando todos os preditores disponíveis (tudo no train_set)
fit_all <- train(
  Survived ~ .,
  data = train_set,
  method = "glm",
  family = "binomial"
)

pred_all <- predict(fit_all, newdata = test_set)
accuracy_all <- mean(pred_all == test_set$Survived)
print(accuracy_all)





library(caret)
set.seed(6)

# Grid de valores k para testar
k_values <- seq(3, 51, 2)

# Treinar o modelo kNN usando caret
fit_knn <- train(
  Survived ~ .,         # fórmula usando todas as variáveis preditoras
  data = train_set,
  method = "knn",
  tuneGrid = data.frame(k = k_values),
  trControl = trainControl(method = "cv")  # validação cruzada
)

# Ver qual valor de k teve melhor performance
best_k <- fit_knn$bestTune$k
print(best_k)




library(ggplot2)

# Usar os resultados do fit_knn que você já treinou
results <- fit_knn$results

# Plotar Accuracy vs k
ggplot(results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "kNN Accuracy vs Number of Neighbors (k)",
       x = "Number of neighbors (k)",
       y = "Accuracy") +
  theme_minimal()



# Previsão com o melhor modelo kNN no conjunto de teste
fit_knn_pred <- predict(fit_knn, newdata = test_set)

# Calcular a acurácia comparando as previsões com os valores reais
accuracy_knn <- mean(fit_knn_pred == test_set$Survived)

# Imprimir a acurácia
print(accuracy_knn)



# Previsão com o melhor modelo kNN no conjunto de teste
fit_knn_pred <- predict(fit_knn, newdata = test_set)

# Calcular a acurácia comparando as previsões com os valores reais
accuracy_knn <- mean(fit_knn_pred == test_set$Survived)

# Imprimir a acurácia
print(accuracy_knn)


print(levels(test_set$Survived))
print(levels(fit_knn_pred))
print(table(fit_knn_pred))
print(table(test_set$Survived))



fit_knn_pred <- factor(fit_knn_pred, levels = levels(test_set$Survived))

library(caret)

set.seed(6)
k_values <- seq(3, 51, 2)

fit_knn <- train(
  Survived ~ .,
  data = train_set,
  method = "knn",
  tuneGrid = data.frame(k = k_values),
  trControl = trainControl(method = "cv")
)

# Previsão no conjunto de teste
fit_knn_pred <- predict(fit_knn, newdata = test_set)

# Ajustar níveis para garantir consistência
fit_knn_pred <- factor(fit_knn_pred, levels = levels(test_set$Survived))

# Calcular acurácia
accuracy_knn <- mean(fit_knn_pred == test_set$Survived)

print(accuracy_knn)


library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
library(caret)

set.seed(8)

# Definir valores de k para testar
k_values <- seq(3, 51, 2)

# Definir controle de treino com validação cruzada 10-fold
trControl_cv <- trainControl(method = "cv", number = 10)

# Treinar modelo kNN com validação cruzada
fit_knn_cv <- train(
  Survived ~ ., 
  data = train_set, 
  method = "knn",
  tuneGrid = data.frame(k = k_values),
  trControl = trControl_cv
)

# Melhor valor de k encontrado
best_k_cv <- fit_knn_cv$bestTune$k
print(best_k_cv)

# Fazer previsões no conjunto de teste com o melhor modelo
pred_knn_cv <- predict(fit_knn_cv, newdata = test_set)

# Garantir que os fatores estão consistentes
pred_knn_cv <- factor(pred_knn_cv, levels = levels(test_set$Survived))

# Calcular acurácia no conjunto de teste
accuracy_knn_cv <- mean(pred_knn_cv == test_set$Survived)
print(accuracy_knn_cv)



titanic_clean <- titanic_clean %>%
  mutate(
    Pclass = factor(Pclass),
    # já tem os outros mutates que você usou
  )





library(titanic)
library(caret)
library(tidyverse)

options(digits = 3)

# Preparar os dados
titanic_clean <- titanic_train %>%
  mutate(
    Survived = factor(Survived),
    Embarked = factor(Embarked),
    Pclass = factor(Pclass),       # <-- aqui converte Pclass para fator
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    FamilySize = SibSp + Parch + 1
  ) %>%
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(8)
train_index <- createDataPartition(titanic_clean$Survived, p = 0.8, list = FALSE)
train_set <- titanic_clean[train_index, ]
test_set <- titanic_clean[-train_index, ]

# Garantir que Pclass é fator em ambos conjuntos
train_set$Pclass <- factor(train_set$Pclass)
test_set$Pclass <- factor(test_set$Pclass, levels = levels(train_set$Pclass))

# Treinar modelo kNN com validação cruzada
k_values <- seq(3, 51, 2)
trControl_cv <- trainControl(method = "cv", number = 10)

fit_knn_cv <- train(
  Survived ~ ., 
  data = train_set,
  method = "knn",
  tuneGrid = data.frame(k = k_values),
  trControl = trControl_cv
)

best_k_cv <- fit_knn_cv$bestTune$k
print(best_k_cv)

# Fazer previsões no conjunto de teste
pred_knn_cv <- predict(fit_knn_cv, newdata = test_set)

# Garantir que os fatores estão consistentes
pred_knn_cv <- factor(pred_knn_cv, levels = levels(test_set$Survived))

# Calcular acurácia
accuracy_knn_cv <- mean(pred_knn_cv == test_set$Survived)
print(accuracy_knn_cv)







library(titanic)
library(caret)
library(tidyverse)

set.seed(6)  # conforme a questão 9a

# Limpeza e preparação dos dados
titanic_clean <- titanic_train %>%
  mutate(
    Survived = factor(Survived),
    Sex = factor(Sex),
    Pclass = factor(Pclass),
    Embarked = factor(Embarked),
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    FamilySize = SibSp + Parch + 1
  ) %>%
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Divisão treino/teste (80/20)
train_index <- createDataPartition(titanic_clean$Survived, p = 0.8, list = FALSE)
train_set <- titanic_clean[train_index, ]
test_set <- titanic_clean[-train_index, ]

# Garantir fatores consistentes no teste (mesmos níveis do treino)
test_set$Pclass <- factor(test_set$Pclass, levels = levels(train_set$Pclass))
test_set$Sex <- factor(test_set$Sex, levels = levels(train_set$Sex))
test_set$Embarked <- factor(test_set$Embarked, levels = levels(train_set$Embarked))
test_set$Survived <- factor(test_set$Survived, levels = levels(train_set$Survived))

# Definição de grid de k
k_values <- seq(3, 51, 2)

# Controle de treino com validação cruzada 10-fold
trControl_cv <- trainControl(method = "cv", number = 10)

# Treinamento do modelo kNN
fit_knn_cv <- train(
  Survived ~ ., 
  data = train_set,
  method = "knn",
  tuneGrid = data.frame(k = k_values),
  trControl = trControl_cv
)

# Melhor valor de k
best_k <- fit_knn_cv$bestTune$k
print(paste("Best k:", best_k))

# Previsão no conjunto de teste
pred_knn_cv <- predict(fit_knn_cv, newdata = test_set)

# Garantir que níveis de fator são iguais
pred_knn_cv <- factor(pred_knn_cv, levels = levels(test_set$Survived))

# Calcular acurácia
accuracy_knn_cv <- mean(pred_knn_cv == test_set$Survived)
print(paste("Accuracy on test set:", round(accuracy_knn_cv, 3)))






library(caret)
library(rpart)

set.seed(10)

# Grid de valores para o parâmetro cp
cp_grid <- expand.grid(cp = seq(0, 0.05, 0.002))

# Treinar modelo de árvore de decisão com cross-validation
fit_rpart <- train(
  Survived ~ ., 
  data = train_set,
  method = "rpart",
  tuneGrid = cp_grid,
  trControl = trainControl(method = "cv", number = 10)
)

# Melhor valor de cp
best_cp <- fit_rpart$bestTune$cp
print(paste("Best cp:", best_cp))

# Previsão no conjunto de teste
pred_rpart <- predict(fit_rpart, newdata = test_set)

# Calcular acurácia
accuracy_rpart <- mean(pred_rpart == test_set$Survived)
print(paste("Accuracy on test set:", round(accuracy_rpart, 3)))




library(caret)
set.seed(14)

# Definir valores de mtry para testar
mtry_values <- seq(1, 7)

# Definir controle de treino com validação cruzada
trControl <- trainControl(method = "cv", number = 10)

# Treinar o modelo Random Forest
fit_rf <- train(
  Survived ~ .,
  data = train_set,
  method = "rf",
  tuneGrid = data.frame(mtry = mtry_values),
  ntree = 100,
  trControl = trControl
)

# Melhor valor de mtry
best_mtry <- fit_rf$bestTune$mtry
print(best_mtry)

# Fazer previsões no conjunto de teste
pred_rf <- predict(fit_rf, newdata = test_set)

# Calcular a acurácia
accuracy_rf <- mean(pred_rf == test_set$Survived)
print(round(accuracy_rf, 3))


library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

fits <- lapply(models, function(model){
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
})

names(fits) <- models






# Criar matriz de predições para o conjunto de teste
predictions <- sapply(fits, function(model) {
  predict(model, newdata = mnist_27$test)
})

# Mostrar as dimensões da matriz de predições
dim(predictions)



# Calcular acurácia para cada modelo no conjunto de teste
accuracies <- colMeans(predictions == mnist_27$test$y)

# Calcular a média das acurácias
mean_accuracy <- mean(accuracies)

# Imprimir a média da acurácia
print(mean_accuracy)




# Converter as predições para valores numéricos (7 e 2)
pred_num <- apply(predictions, 2, function(x) as.numeric(as.character(x)))

# Calcular a soma das predições para cada linha
sum_preds <- rowSums(pred_num == 7)

# Votar 7 se mais da metade dos modelos preveem 7, senão votar 2
ensemble_pred <- ifelse(sum_preds > (ncol(pred_num) / 2), 7, 2)

# Calcular a acurácia do ensemble comparando com os valores reais
ensemble_accuracy <- mean(ensemble_pred == mnist_27$test$y)

# Imprimir acurácia do ensemble
print(ensemble_accuracy)




# Acurrácias individuais
acc <- c(glm=0.825, lda=0.825, naive_bayes=0.764, knn=0.759, gamLoess=0.741, qda=0.731, rf=0.828)

# Exemplo de acurácia do ensemble (confirma o valor real que você tem)
ensemble_accuracy <- 0.84

# Quantos modelos são melhores que o ensemble
better_than_ensemble <- acc > ensemble_accuracy
sum(better_than_ensemble)

# Quais modelos são melhores que o ensemble
names(acc)[better_than_ensemble]


mean_accuracy <- mean(sapply(fits, function(x) min(x$results$Accuracy)))
print(round(mean_accuracy, 3))









# Supondo que você já tenha a lista 'fits' com todos os modelos treinados
# E que 'mnist_27$testy' seja o vetor de valores reais no conjunto de teste

# Filtrar modelos com acurácia mínima >= 0.8
good_models <- fits[sapply(fits, function(x) min(x$results$Accuracy)) >= 0.8]

# Fazer previsões para cada modelo filtrado no conjunto de teste
predictions <- sapply(good_models, function(model) predict(model, newdata = mnist_27$test))

# Converter as previsões para numérico (caso não esteja)
predictions <- apply(predictions, 2, function(col) as.numeric(as.character(col)))

# Votar 7 se 50% ou mais modelos votarem 7, senão 2
ensemble_pred <- apply(predictions, 1, function(row) {
  if(mean(row == 7) >= 0.5) 7 else 2
})

# Calcular acurácia do ensemble
ensemble_accuracy <- mean(ensemble_pred == mnist_27$testy)
print(round(ensemble_accuracy, 3))




# Filtrar modelos com acurácia mínima >= 0.8
good_models <- fits[sapply(fits, function(x) min(x$results$Accuracy)) >= 0.8]
cat("Number of good models:", length(good_models), "\n")

# Fazer previsões para cada modelo filtrado no conjunto de teste
predictions <- sapply(good_models, function(model) predict(model, newdata = mnist_27$test))
cat("Dimension of predictions matrix:", dim(predictions), "\n")

# Converter as previsões para numérico (caso não esteja)
predictions_num <- apply(predictions, 2, function(col) as.numeric(as.character(col)))

# Verificar NA nas previsões
cat("Number of NA in predictions:", sum(is.na(predictions_num)), "\n")

# Garantir que os valores reais estão no mesmo formato
true_values <- mnist_27$testy
cat("Unique true values:", unique(true_values), "\n")

# Votar 7 se 50% ou mais modelos votarem 7, senão 2
ensemble_pred <- apply(predictions_num, 1, function(row) {
  if(mean(row == 7) >= 0.5) 7 else 2
})

# Verificar NA na votação
cat("Number of NA in ensemble predictions:", sum(is.na(ensemble_pred)), "\n")

# Calcular acurácia do ensemble (somente se não houver NA)
if(sum(is.na(ensemble_pred)) == 0){
  ensemble_accuracy <- mean(ensemble_pred == true_values)
  cat("Ensemble accuracy:", round(ensemble_accuracy, 3), "\n")
} else {
  cat("Erro: predições com NA, não foi possível calcular a acurácia.\n")
}





cat("Class of ensemble_pred:", class(ensemble_pred), "\n")
cat("Unique values of ensemble_pred:", unique(ensemble_pred), "\n")

cat("Class of true_values:", class(true_values), "\n")
cat("Unique values of true_values:", unique(true_values), "\n")


ensemble_pred <- as.factor(ensemble_pred)
true_values <- as.factor(true_values)





ensemble_pred <- as.numeric(as.character(ensemble_pred))
true_values <- as.numeric(as.character(true_values))




print(ensemble_pred == true_values)








# Exemplo completo para calcular acurácia do ensemble

# Suponha que você tenha as predições do ensemble (vector) em 'ensemble_pred'
# E que o vetor de valores reais esteja em 'true_values'

# Ajustar classes para comparação
ensemble_pred <- as.factor(ensemble_pred)
true_values <- as.factor(mnist_27$testy)  # Use o nome correto do vetor de valores reais

# Verificar tamanhos e tipos
cat("Length of ensemble_pred:", length(ensemble_pred), "\n")
cat("Length of true_values:", length(true_values), "\n")
cat("Class of ensemble_pred:", class(ensemble_pred), "\n")
cat("Class of true_values:", class(true_values), "\n")

# Verificar se valores únicos estão iguais
cat("Unique levels ensemble_pred:", levels(ensemble_pred), "\n")
cat("Unique levels true_values:", levels(true_values), "\n")

# Calcular acurácia (proporção de acertos)
accuracy <- mean(ensemble_pred == true_values)
cat("Accuracy of ensemble:", accuracy, "\n")




library(tidyverse)
library(lubridate)
library(dslabs)

data("movielens")

# Extrair o ano do título
movielens <- movielens %>%
  mutate(year = str_extract(title, "\\(\\d{4}\\)")) %>%
  mutate(year = as.integer(str_remove_all(year, "[()]")))

# Contar número de avaliações por filme
ratings_per_movie <- movielens %>%
  group_by(movieId, year) %>%
  summarise(n_ratings = n(), .groups = "drop")

# Criar boxplot com transformação sqrt no eixo y
ratings_per_movie %>%
  ggplot(aes(x = factor(year), y = sqrt(n_ratings))) +
  geom_boxplot() +
  labs(x = "Year", y = "Square Root of Number of Ratings") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Calcular a mediana do número de avaliações por ano
median_ratings_by_year <- ratings_per_movie %>%
  group_by(year) %>%
  summarise(median_ratings = median(n_ratings), .groups = "drop") %>%
  arrange(desc(median_ratings))

# Exibir o topo da lista
head(median_ratings_by_year, 1)


# Remover filmes sem ano
ratings_per_movie_clean <- ratings_per_movie %>%
  filter(!is.na(year))

# Calcular a mediana do número de avaliações por ano (com dados limpos)
median_ratings_by_year <- ratings_per_movie_clean %>%
  group_by(year) %>%
  summarise(median_ratings = median(n_ratings), .groups = "drop") %>%
  arrange(desc(median_ratings))

# Exibir o topo da lista
head(median_ratings_by_year, 1)




ratings_per_movie_clean <- ratings_per_movie %>%
  filter(!is.na(year))



median_ratings_by_year <- ratings_per_movie_clean %>%
  group_by(year) %>%
  summarise(median_ratings = median(n_ratings), .groups = "drop") %>%
  arrange(desc(median_ratings))

head(median_ratings_by_year, 1)


# Remover NAs e calcular corretamente o ano com maior mediana
ratings_per_movie_clean <- ratings_per_movie %>%
  filter(!is.na(year))

median_ratings_by_year <- ratings_per_movie_clean %>%
  group_by(year) %>%
  summarise(median_ratings = median(n_ratings), .groups = "drop") %>%
  arrange(desc(median_ratings))

# Mostrar o ano com a maior mediana
head(median_ratings_by_year, 1)





library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Extrair o ano do título
movielens <- movielens %>%
  mutate(year = str_extract(title, "\\(\\d{4}\\)")) %>%
  mutate(year = as.integer(str_remove_all(year, "[()]")))

# Contar número de avaliações por filme
ratings_per_movie <- movielens %>%
  group_by(movieId, year) %>%
  summarise(n_ratings = n(), .groups = "drop")

# Remover filmes sem ano
ratings_per_movie_clean <- ratings_per_movie %>%
  filter(!is.na(year))

# Calcular mediana por ano
median_ratings_by_year <- ratings_per_movie_clean %>%
  group_by(year) %>%
  summarise(median_ratings = median(n_ratings), .groups = "drop") %>%
  arrange(desc(median_ratings))

# Mostrar o ano com maior mediana
head(median_ratings_by_year, 1)


mutate(year = str_extract(title, "\\(\\d{4}\\)")) %>%
  mutate(year = as.integer(str_remove_all(year, "[()]")))



library(tidyverse)
library(lubridate)
library(dslabs)

# Carregar os dados
data("movielens")

# Extrair e salvar corretamente o ano no objeto
movielens <- movielens %>%
  mutate(year = str_extract(title, "\\(\\d{4}\\)")) %>%
  mutate(year = as.integer(str_remove_all(year, "[()]")))

# Contar número de avaliações por filme
ratings_per_movie <- movielens %>%
  group_by(movieId, year) %>%
  summarise(n_ratings = n(), .groups = "drop")

# Remover filmes sem ano
ratings_per_movie_clean <- ratings_per_movie %>%
  filter(!is.na(year))

# Calcular mediana por ano
median_ratings_by_year <- ratings_per_movie_clean %>%
  group_by(year) %>%
  summarise(median_ratings = median(n_ratings), .groups = "drop") %>%
  arrange(desc(median_ratings))

# Mostrar o ano com maior mediana
head(median_ratings_by_year, 1)
































































































































