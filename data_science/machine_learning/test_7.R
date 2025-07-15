# 1. Carregar as bibliotecas e dados (certifique-se de ter esses pacotes instalados)
# install.packages(c("titanic", "caret", "tidyverse", "rpart"))
library(titanic)
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# Limpar e preparar os dados conforme o script original
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA Age to median
         FamilySize = SibSp + Parch + 1) %>% # count family members
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Certifique-se de que Survived seja um fator com 2 níveis (0 e 1)
# O pacote caret espera que o fator de resultado seja codificado como 0 e 1, ou "No" e "Yes".
# 0 = Died, 1 = Survived
levels(titanic_clean$Survived) <- c("Died", "Survived")


# 2. Construir os modelos
# Modelo de Sexo
model_sex <- rpart(Survived ~ Sex, data = titanic_clean, method = "class")

# Modelo de Classe
model_class <- rpart(Survived ~ Pclass, data = titanic_clean, method = "class")

# Modelo Combinado (Sex e Class)
model_combined <- rpart(Survived ~ Sex + Pclass, data = titanic_clean, method = "class")


# 3. Fazer previsões e 4. Converter previsões para fatores
# Previsões para o modelo de Sexo
predictions_sex <- predict(model_sex, titanic_clean, type = "class")

# Previsões para o modelo de Classe
predictions_class <- predict(model_class, titanic_clean, type = "class")

# Previsões para o modelo Combinado
predictions_combined <- predict(model_combined, titanic_clean, type = "class")


# 5. Calcular as pontuações F1 usando F_meas()
# A função F_meas() do pacote 'caret' espera (data, reference).
# 'data' são as previsões do modelo e 'reference' são os valores reais (Survived).

f1_sex <- F_meas(data = predictions_sex, reference = titanic_clean$Survived, relevant = "Survived")
f1_class <- F_meas(data = predictions_class, reference = titanic_clean$Survived, relevant = "Survived")
f1_combined <- F_meas(data = predictions_combined, reference = titanic_clean$Survived, relevant = "Survived")

# Imprimir os resultados
cat("Pontuação F1 para o modelo de Sexo:", f1_sex, "\n")
cat("Pontuação F1 para o modelo de Classe:", f1_class, "\n")
cat("Pontuação F1 para o modelo Combinado (Sex + Pclass):", f1_combined, "\n")

# Encontrar o modelo com a maior pontuação F1 e seu valor
f1_scores <- c("sex" = f1_sex, "class" = f1_class, "combined" = f1_combined)
max_f1_model <- names(which.max(f1_scores))
max_f1_value <- max(f1_scores)

cat("\nO modelo com a maior pontuação F1 é:", max_f1_model, "\n")
cat("O valor máximo da pontuação F1 é:", round(max_f1_value, 3), "\n") # Arredondando para 3 casas decimais