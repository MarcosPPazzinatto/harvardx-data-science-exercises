set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))


set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))



schools %>% top_n(10, quality) %>% arrange(desc(quality))


scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))



# Ordenar pela média da nota (score), da maior para a menor
top_10 <- schools %>%
  arrange(desc(score)) %>%
  slice(1:10)

# Mostrar apenas id, size, e score
top_10 %>%
  select(id, size, score)



# ID da 10ª escola
top_10$`id`[10]

# Nota média da 10ª escola
top_10$`score`[10]



# 1) Mediana do tamanho de todas as escolas
median_all <- median(schools$size)

# 2) Mediana do tamanho das 10 melhores escolas
median_top10 <- median(top_10$size)

# Ver resultados
median_all
median_top10



# Selecionar as 10 piores escolas pelo score
bottom_10 <- schools %>%
  arrange(score) %>%
  slice(1:10)

# Calcular a mediana do tamanho dessas escolas
median(bottom_10$size)


# Média geral das notas
overall <- mean(sapply(scores, mean))

# Parâmetro de regularização
alpha <- 25

# Calcular score regularizado para cada escola
schools <- schools %>%
  mutate(
    reg_score = overall + (score - overall) * size / (size + alpha)
  )

# Top 10 escolas com score regularizado
top_10_reg <- schools %>%
  arrange(desc(reg_score)) %>%
  slice(1:10)

# Mostrar ID da 1ª e reg_score da 10ª
top_10_reg$`id`[1]       # ID da melhor escola
top_10_reg$`reg_score`[10]  # Nota regularizada da 10ª escola




# Qualidade real das escolas já está no data frame
true_quality <- schools$quality
overall <- mean(sapply(scores, mean))

# Função para calcular RMSE dado um alpha
rmse <- function(alpha) {
  reg_score <- overall + (schools$score - overall) * schools$size / (schools$size + alpha)
  sqrt(mean((reg_score - true_quality)^2))
}

# Testar alphas de 10 a 250
alphas <- 10:250
rmse_values <- sapply(alphas, rmse)

# Encontrar o alpha com menor RMSE
best_alpha <- alphas[which.min(rmse_values)]
best_alpha






# Recalcular score regularizado com o melhor alpha
schools <- schools %>%
  mutate(
    reg_score_best = overall + (score - overall) * size / (size + best_alpha)
  )

# Top 10 escolas com melhor reg_score
top10_best <- schools %>%
  arrange(desc(reg_score_best)) %>%
  slice(1:10)

# Resultados para preencher:
top10_best$`id`[1]         # ID da melhor escola
top10_best$`reg_score_best`[10]  # Score da 10ª escola





# Qualidade verdadeira
true_quality <- schools$quality

# Função sem centralizar no overall
rmse_no_center <- function(alpha) {
  reg_score <- schools$score * schools$size / (schools$size + alpha)
  sqrt(mean((reg_score - true_quality)^2))
}

# Testar alpha de 10 a 250
alphas <- 10:250
rmse_values <- sapply(alphas, rmse_no_center)

# Melhor alpha
best_alpha_no_center <- alphas[which.min(rmse_values)]
best_alpha_no_center


# Gerar dados para o exercício de Matrix Factorization
set.seed(1987)
n <- 100
k <- 8

# Covariância entre Math, Science, Arts
Sigma <- 64 * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)

# Gerar matriz m com base em distribuição normal multivariada
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)

# Ordenar os alunos do melhor para o pior (com base na média das 3 notas)
m <- m[order(rowMeans(m), decreasing = TRUE), ]

# Gerar matriz de pesos para as disciplinas
loadings <- matrix(rep(1, 3 * k * 1), nrow = 3, ncol = 3 * k)  # 3 x 24

# Construir y com estrutura + ruído
y <- m %*% loadings + matrix(rnorm(n * k * 3), n, k * 3)

# Nomear colunas por área
colnames(y) <- c(
  paste(rep("Math", k), 1:k, sep = "_"),
  paste(rep("Science", k), 1:k, sep = "_"),
  paste(rep("Arts", k), 1:k, sep = "_")
)

# Aplicar SVD
s <- svd(y)

# Soma dos quadrados das colunas originais
ss_y <- colSums(y^2)

# Soma dos quadrados das colunas transformadas por V
yv <- y %*% s$v
ss_yv <- colSums(yv^2)

# Confirmar igualdade
sum(ss_y)
sum(ss_yv)


format(sum(ss_y), digits = 10)



plot(ss_y, type = "b", col = "blue", main = "Sums of Squares: ss_y vs ss_yv", ylab = "Sum of Squares", xlab = "Column")
lines(ss_yv, type = "b", col = "red")
legend("topright", legend = c("ss_y", "ss_yv"), col = c("blue", "red"), lty = 1)





# Soma total da variância
total_var <- sum(s$d^2)

# Variância explicada pelas 3 primeiras componentes
var_top3 <- sum(s$d[1:3]^2)

# Proporção explicada (em decimal)
prop <- var_top3 / total_var
prop



options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)



dim(brca$x)


mean(brca$y == "M")

which.max(colMeans(brca$x))

which.min(colSds(brca$x))



x_centered <- sweep(brca$x, 2, colMeans(brca$x))      # subtrai a média
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")  # divide pelo desvio padrão


sd(x_scaled[,1])

median(x_scaled[,1])



summary(pca)$importance[2, 1]

pca <- prcomp(x_scaled)

summary(pca)$importance[2, 1]   # proporção explicada pelo 1º componente
which(cumsum(summary(pca)$importance[2, ]) >= 0.9)[1]  # quantos PCs explicam 90%




pca <- prcomp(x_scaled)
pc_scores <- data.frame(pca$x, type = brca$y)



library(ggplot2)
pc_scores %>%
  gather(key = "PC", value = "score", -type) %>%
  filter(PC %in% paste0("PC", 1:10)) %>%
  ggplot(aes(x = type, y = score, fill = type)) +
  geom_boxplot() +
  facet_wrap(~PC, scales = "free_y")


confusionMatrix(predictions_log, test_y)$overall["Accuracy"]









set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]




















options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)


set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]



# Preparar os dados e bibliotecas
library(dslabs)
library(caret)
library(tidyverse)
library(matrixStats)
data(brca)

# Dividir os dados
x <- brca$x
y <- brca$y

# Separar treino/teste
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
x_train <- x[-test_index, ]
y_train <- y[-test_index]
x_test <- x[test_index, ]
y_test <- y[test_index]

# Treinar modelo de regressão logística
set.seed(1)
fit_glm <- train(x_train, y_train, method = "glm", family = "binomial")

# Prever no conjunto de teste
y_hat <- predict(fit_glm, x_test)

# Calcular acurácia
accuracy <- mean(y_hat == y_test)
accuracy









# Carregar bibliotecas e dados
library(dslabs)
library(caret)
library(tidyverse)
library(matrixStats)
library(gam)        # necessário para gamLoess
data(brca)

# Preparar os dados
x <- brca$x
y <- brca$y

# Separar treino/teste
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
x_train <- x[-test_index, ]
y_train <- y[-test_index]
x_test <- x[test_index, ]
y_test <- y[test_index]

# Treinar modelo Loess
set.seed(5)
fit_loess <- train(x_train, y_train, method = "gamLoess")

# Prever no conjunto de teste
y_hat_loess <- predict(fit_loess, x_test)

# Calcular acurácia
mean(y_hat_loess == y_test)





# Carregar bibliotecas
library(dslabs)
library(caret)
library(tidyverse)
library(matrixStats)
library(gam)  # importante para o gamLoess
data(brca)

# Preparar os dados
x <- brca$x
y <- brca$y

# Dividir entre treino e teste
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
x_train <- x[-test_index, ]
y_train <- y[-test_index]
x_test <- x[test_index, ]
y_test <- y[test_index]

# Treinar modelo gamLoess
set.seed(5)
fit_loess <- train(x_train, y_train, method = "gamLoess")

# Prever no conjunto de teste
y_hat_loess <- predict(fit_loess, x_test)

# Mostrar resultado com mais precisão
options(digits = 10)
accuracy <- mean(y_hat_loess == y_test)
accuracy





# Carregar bibliotecas
library(dslabs)
library(caret)
library(tidyverse)
library(matrixStats)
data(brca)

# Preparar os dados
x <- brca$x
y <- brca$y

# Separar treino/teste
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
x_train <- x[-test_index, ]
y_train <- y[-test_index]
x_test <- x[test_index, ]
y_test <- y[test_index]

# Treinar modelo KNN
set.seed(7)
fit_knn <- train(
  x_train, y_train, 
  method = "knn", 
  tuneGrid = data.frame(k = seq(3, 21, 2))
)

# Melhor valor de k encontrado
fit_knn$bestTune

# Previsões no conjunto de teste
y_hat_knn <- predict(fit_knn, x_test)

# Acurácia
options(digits = 3)
mean(y_hat_knn == y_test)


fit_knn$bestTune

mean(y_hat_knn == y_test)































