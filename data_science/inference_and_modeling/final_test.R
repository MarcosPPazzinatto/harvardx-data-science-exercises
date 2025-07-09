library(dslabs)
library(dplyr)

data("brexit_polls")

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)

# Agora calcular os valores
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)


brexit_polls[1, ]


# Extraindo a primeira linha
first_poll <- brexit_polls[1, ]

# Calculando x_hat
x_hat <- (first_poll$spread + 1) / 2

# Tamanho da amostra
N <- first_poll$samplesize

# Erro padrão
se_hat <- sqrt(x_hat * (1 - x_hat) / N)

# z para 95% de confiança
z <- qnorm(0.975)

# Limites do intervalo
ci_lower <- x_hat - z * se_hat
ci_upper <- x_hat + z * se_hat

# Mostrar valores
x_hat
se_hat
ci_lower
ci_upper




# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481


library(dplyr)
library(dslabs)
data("brexit_polls")

# Adiciona x_hat
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)

# Filtro para apenas pesquisas com enddate em junho de 2016
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01")

# Número total de pesquisas em junho
nrow(june_polls)

# Calcular se_hat para cada pesquisa
june_polls <- june_polls %>%
  mutate(
    se_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
    se_spread = 2 * se_hat,
    lower = spread - qnorm(0.975) * se_spread,
    upper = spread + qnorm(0.975) * se_spread,
    covers_0 = lower <= 0 & upper >= 0,
    covers_remain = lower > 0,
    d = -0.038,
    covers_d = lower <= d & upper >= d
  )

# Respostas
nrow(june_polls)                            # Quantidade total
mean(june_polls$covers_0)                  # Proporção cobrindo 0
mean(june_polls$covers_remain)             # Proporção com IC totalmente acima de 0
mean(june_polls$covers_d)                  # Proporção cobrindo o d verdadeiro







library(dplyr)

# Agrupar por pollster e calcular taxa de acerto (hit rate)
pollster_hits <- june_polls %>%
  group_by(pollster) %>%
  summarize(
    num_polls = n(),
    hit_rate = mean(covers_d)
  ) %>%
  arrange(desc(hit_rate))

pollster_hits






library(ggplot2)

ggplot(june_polls, aes(x = poll_type, y = spread)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Spread by Poll Type", y = "Spread", x = "Poll Type")



# Passo 1: combinar os dados por tipo de pesquisa
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(
    N = sum(samplesize),
    spread = sum(spread * samplesize) / N,
    p_hat = (spread + 1) / 2
  ) %>%
  mutate(
    se_p_hat = sqrt(p_hat * (1 - p_hat) / N),
    se_spread = 2 * se_p_hat,
    lower = spread - qnorm(0.975) * se_spread,
    upper = spread + qnorm(0.975) * se_spread
  )

combined_by_type





library(dslabs)
library(dplyr)

data("brexit_polls")

# Preparar coluna x_hat
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)

# Calcular intervalo e hit
brexit_hit <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(
    se_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
    se_spread = 2 * se_hat,
    lower = spread - qnorm(0.975) * se_spread,
    upper = spread + qnorm(0.975) * se_spread,
    hit = lower <= -0.038 & upper >= -0.038
  )

# Tabela 2x2
table_hit <- table(brexit_hit$poll_type, brexit_hit$hit)

# Teste qui-quadrado
chisq_test_result <- chisq.test(table_hit)
chisq_test_result





library(dplyr)
library(dslabs)
data("brexit_polls")

# Valor verdadeiro do spread
d <- -0.038

# Criar x_hat
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)

# Filtrar e calcular hits
brexit_hit <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(
    se_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
    se_spread = 2 * se_hat,
    lower = spread - qnorm(0.975) * se_spread,
    upper = spread + qnorm(0.975) * se_spread,
    hit = lower <= d & upper >= d
  )

# Criar tabela 2x2
table_hit <- table(brexit_hit$poll_type, brexit_hit$hit)

# Teste Qui-quadrado
chisq_result <- chisq.test(table_hit)
chisq_result


chisq_result$p.value


hit = lower <= -0.038 & upper >= -0.038

hit = lower <= d & upper >= d





library(dplyr)
library(dslabs)
data("brexit_polls")
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Filtrar e calcular hits
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

# Criar tabela 2x2
table_hit <- table(brexit_hit$poll_type, brexit_hit$hit)

# Teste Qui-quadrado
chisq_result <- chisq.test(table_hit)
chisq_result



table_hit


# Atribuir valores diretamente
online_hit     <- table_hit["Online", "TRUE"]
online_miss    <- table_hit["Online", "FALSE"]
telephone_hit  <- table_hit["Telephone", "TRUE"]
telephone_miss <- table_hit["Telephone", "FALSE"]

# Calcular as odds
odds_online    <- online_hit / online_miss
odds_telephone <- telephone_hit / telephone_miss

# Odds ratio
odds_ratio     <- odds_online / odds_telephone

# Mostrar valores
odds_online
odds_telephone
odds_ratio


