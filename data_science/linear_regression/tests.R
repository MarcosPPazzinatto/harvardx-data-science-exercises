library(dslabs)
library(dplyr)

data("research_funding_rates")

# Soma total de homens e mulheres
men_totals <- research_funding_rates %>%
  summarize(applicants = sum(applications_men), awarded = sum(awards_men))

women_totals <- research_funding_rates %>%
  summarize(applicants = sum(applications_women), awarded = sum(awards_women))

# Calcula o número de não premiados
men_not_awarded <- men_totals$applicants - men_totals$awarded
women_not_awarded <- women_totals$applicants - women_totals$awarded

# Exibe os resultados
men_not_awarded
women_not_awarded



# Calcula as porcentagens
percent_men_awarded <- (men_totals$awarded / men_totals$applicants) * 100
percent_women_awarded <- (women_totals$awarded / women_totals$applicants) * 100

# Exibe com 1 casa decimal
round(percent_men_awarded, 1)
round(percent_women_awarded, 1)





# Supondo que você já tem:
# men_totals$applicants, men_totals$awarded
# women_totals$applicants, women_totals$awarded

# Calcula os não premiados novamente (caso precise)
men_not_awarded <- men_totals$applicants - men_totals$awarded
women_not_awarded <- women_totals$applicants - women_totals$awarded

# Monta a tabela 2x2
two_by_two <- matrix(c(
  men_totals$awarded, men_not_awarded,
  women_totals$awarded, women_not_awarded
), nrow = 2, byrow = TRUE)

# Nomeia as linhas e colunas (opcional, mas útil)
dimnames(two_by_two) <- list(
  Gender = c("Men", "Women"),
  Outcome = c("Awarded", "Not Awarded")
)



# Teste de qui-quadrado
chi_result <- chisq.test(two_by_two)

# Exibe o p-valor
chi_result$p.value

library(broom)
tidy(chi_result)

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")
dat



# Carrega os dados
library(dslabs)
data("research_funding_rates")

# Visualiza os dados
research_funding_rates



library(Lahman)
library(dplyr)

Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G,
         AB_per_game = AB / G) %>%
  summarize(cor = cor(R_per_game, AB_per_game)) %>%
  pull(cor)


Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G,
         errors_per_game = E / G) %>%
  summarize(cor = cor(win_rate, errors_per_game)) %>%
  pull(cor)


Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(doubles_per_game = X2B / G,
         triples_per_game = X3B / G) %>%
  summarize(cor = cor(doubles_per_game, triples_per_game)) %>%
  pull(cor)



set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)



mean_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)

mean_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)

correlation <- cor(female_heights$mother, female_heights$daughter)


mean_mother

sd_mother

mean_daughter

sd_daughter

correlation
















