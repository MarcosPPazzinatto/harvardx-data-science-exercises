library(gtools)
nrow(permutations(8, 3))


nrow(permutations(3, 3))


set.seed(1)  # fixar a semente

runners <- c("Jamaica", "Jamaica", "Jamaica",
             "USA", "Ecuador", "Netherlands", "France", "South Africa")

# Simular 10.000 sorteios de 3 medalhistas
results <- replicate(10000, {
  sample <- sample(runners, 3)
  all(sample == "Jamaica")  # retorna TRUE se os 3 forem da Jamaica
})

# Calcular a proporção de TRUEs
mean(results)


# Número fixo de combinações de sides e drinks
side_combinations <- choose(6, 2)  # 15
drink_options <- 3

# Função que calcula o total de combinações
meal_combinations <- function(entrees) {
  entrees * side_combinations * drink_options
}

# Testar de 1 a 12
results <- sapply(1:12, meal_combinations)
results

# Entrées e drinks fixos
entrees <- 6
drinks <- 3

# Função para número de sides variando de 2 a 12
meal_combinations <- function(num_sides) {
  entrees * choose(num_sides, 2) * drinks
}

# Aplicar de 2 a 12
results <- sapply(2:12, meal_combinations)
results




library(dplyr)

esoph %>%
  summarise(total_controls = sum(ncontrols))



library(dplyr)

esoph %>%
  filter(alcgp == "120+") %>%
  summarise(
    total_cases = sum(ncases),
    total_controls = sum(ncontrols),
    probability = total_cases / (total_cases + total_controls)
  )

unique(esoph$alcgp)

esoph %>%
  filter(alcgp == "120+") %>%
  summarise(
    total_cases = sum(ncases),
    total_controls = sum(ncontrols),
    probability = total_cases / (total_cases + total_controls)
  )


esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarise(
    total_cases = sum(ncases),
    total_controls = sum(ncontrols),
    probability = total_cases / (total_cases + total_controls)
  )

esoph %>%
  summarise(
    total_cases = sum(ncases),
    cases_10g_or_more = sum(ncases[tobgp %in% c("10-19", "20+")]),
    probability = cases_10g_or_more / total_cases
  )


esoph %>%
  filter(trimws(tobgp) %in% c("10-19", "20+")) %>%
  summarise(
    cases_10g_or_more = sum(ncases)
  ) -> part

total <- sum(esoph$ncases)

part$cases_10g_or_more / total

cases_10g_or_more = sum(ncases[trimws(tobgp) %in% c("10-19", "20+")])


esoph %>%
  filter(trimws(tobgp) %in% c("10-19", "20+")) %>%
  summarise(
    cases_10g_or_more = sum(ncases)
  ) -> part

total <- sum(esoph$ncases)

part$cases_10g_or_more / total


unique(esoph$tobgp)


esoph %>%
  summarise(
    total_cases = sum(ncases),
    cases_10g_or_more = sum(ncases[tobgp %in% c("10-19", "20-29", "30+")]),
    probability = cases_10g_or_more / total_cases
  )



esoph %>%
  summarise(
    total_controls = sum(ncontrols),
    controls_10g_or_more = sum(ncontrols[tobgp %in% c("10-19", "20-29", "30+")]),
    probability = controls_10g_or_more / total_controls
  )


esoph %>%
  summarise(
    total_cases = sum(ncases),
    cases_highest_alcohol = sum(ncases[alcgp == "120+"]),
    probability = cases_highest_alcohol / total_cases
  )



esoph %>%
  summarise(
    total_cases = sum(ncases),
    cases_highest_tobacco = sum(ncases[tobgp == "30+"]),
    probability = cases_highest_tobacco / total_cases
  )

esoph %>%
  summarise(
    total_cases = sum(ncases),
    cases_both_high = sum(ncases[alcgp == "120+" & tobgp == "30+"]),
    probability = cases_both_high / total_cases
  )



# Total de casos
total_cases <- sum(esoph$ncases)

# A: casos no grupo mais alto de álcool
a <- sum(esoph$ncases[esoph$alcgp == "120+"]) / total_cases

# B: casos no grupo mais alto de tabaco
b <- sum(esoph$ncases[esoph$tobgp == "30+"]) / total_cases

# A ∩ B: casos nos dois grupos
a_and_b <- sum(esoph$ncases[esoph$alcgp == "120+" & esoph$tobgp == "30+"]) / total_cases

# Probabilidade final
p_union <- a + b - a_and_b
p_union



esoph %>%
  summarise(
    total_controls = sum(ncontrols),
    controls_highest_alcohol = sum(ncontrols[alcgp == "120+"]),
    probability = controls_highest_alcohol / total_controls
  )

esoph %>%
  summarise(
    total_controls = sum(ncontrols),
    controls_highest_tobacco = sum(ncontrols[tobgp == "30+"]),
    probability = controls_highest_tobacco / total_controls
  )

esoph %>%
  summarise(
    total_controls = sum(ncontrols),
    controls_both_high = sum(ncontrols[alcgp == "120+" & tobgp == "30+"]),
    probability = controls_both_high / total_controls
  )





























































