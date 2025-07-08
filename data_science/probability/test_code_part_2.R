set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)
mean(act_scores)

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)

sd(act_scores)

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)


sum(act_scores >= 36)

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)

mean(act_scores > 30)


set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)


mean(act_scores <= 10)

f_x <- dnorm(x, mean = 20.9, sd = 5.7)


1 - pnorm(2)


qnorm(0.975, mean = 20.874, sd = 5.716)

qnorm(0.95, mean = 20.874, sd = 5.716)


qnorm(0.95, 20.874, 5.716)
# Resultado ≈ 30.3


qnorm(0.95, mean = 20.9, sd = 5.7)


p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)

sample_quantiles["0.82"] = 25.99903
sample_quantiles["0.83"] = 26.37566


p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, mean = 20.9, sd = 5.7)
sample_quantiles <- quantile(act_scores, p)



set.seed(21, sample.kind = "Rounding")  # para versões R >= 3.6

# parâmetros
n <- 10000
B <- 44
score <- replicate(n, {
  x <- sample(c(1, -0.25), B, replace = TRUE, prob = c(0.2, 0.8))
  sum(x)
})

mean(score >= 8)


pnorm(0, mean = -65.8, sd = 52.91)


pnorm(0, mean = -65.8, sd = 52.91)
# [1] 0.8931805



library(dslabs)
data(death_prob)

p <- death_prob %>%
  filter(age == 50, sex == "Female") %>%
  pull(prob)
p


























































