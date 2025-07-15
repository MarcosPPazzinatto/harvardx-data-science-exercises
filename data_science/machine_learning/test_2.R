library(tidyverse)
library(dslabs)
data("movielens")

# Contar número de ratings por filme e por ano
ratings_per_movie <- movielens %>%
  filter(!is.na(year)) %>%         # garantimos que o year existe
  group_by(movieId, year) %>%
  summarize(n_ratings = n(), .groups = "drop")




ratings_per_movie %>%
  ggplot(aes(x = factor(year), y = n_ratings)) +
  geom_boxplot() +
  scale_y_sqrt() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = "Ano de Lançamento",
    y = "Número de Avaliações (transformado √)",
    title = "Distribuição de Avaliações por Ano"
  )



ratings_per_movie %>%
  group_by(year) %>%
  summarize(median_ratings = median(n_ratings)) %>%
  arrange(desc(median_ratings)) %>%
  slice(1)






library(tidyverse)
library(dslabs)
data("movielens")

# 1) Calcular stats por filme
movie_stats <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId, title, year) %>%
  summarize(
    n_ratings = n(),
    avg_rating = mean(rating),
    .groups = "drop"
  ) %>%
  mutate(
    ratings_per_year = n_ratings / (2018 - year)
  )

# 2) Top 25 filmes por ratings_per_year
top25 <- movie_stats %>%
  arrange(desc(ratings_per_year)) %>%
  slice(1:25)

# 3) Média de rating de The Shawshank Redemption dentro do top25
shawshank_avg <- top25 %>%
  filter(str_detect(title, "Shawshank Redemption")) %>%
  pull(avg_rating)

# 4) Ratings_per_year de Forrest Gump
forrest_rpy <- movie_stats %>%
  filter(str_detect(title, "Forrest Gump")) %>%
  pull(ratings_per_year)

# Imprimir resultados
cat("Média de rating de The Shawshank Redemption (entre top 25):", shawshank_avg, "\n")
cat("Ratings por ano de Forrest Gump:", forrest_rpy, "\n")



movie_stats %>% 
  filter(year >= 1993) %>%
  ggplot(aes(x = ratings_per_year, y = avg_rating)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    x = "Ratings por ano",
    y = "Média de avaliação",
    title = "Relação entre frequência de ratings e média de ratings"
  )


library(tidyverse)
library(dslabs)
data("movielens")

# Calcular média e erro padrão por categoria de gênero (sem separar os múltiplos)
genre_stats <- movielens %>%
  group_by(genres) %>%
  summarize(
    n = n(),
    avg_rating = mean(rating),
    se = sd(rating) / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(n > 1000)

# Plotar o gráfico com barras de erro
genre_stats %>%
  ggplot(aes(x = reorder(genres, avg_rating), y = avg_rating)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = avg_rating - se, ymax = avg_rating + se), width = 0.2) +
  coord_flip() +
  labs(
    x = "Genre",
    y = "Average Rating",
    title = "Average Rating by Genre (with SE)"
  )




genre_stats %>%
  arrange(avg_rating) %>%
  slice(1)



# Ordenar pela média da nota (score), da maior para a menor
top_10 <- schools %>%
  arrange(desc(score)) %>%
  slice(1:10)

# Mostrar apenas id, size, e score
top_10 %>%
  select(id, size, score)











