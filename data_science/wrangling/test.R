url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- readr::read_csv(url, col_names = FALSE)
dim(dat)



install.packages("Lahman")
library(dplyr)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()


top_names <- top %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, HR)


# 1. Filtra os prêmios de 2016
awards_2016 <- AwardsPlayers %>% filter(yearID == 2016)

# 2. Compara com os playerIDs dos top 10
intersect(top_names$playerID, awards_2016$playerID)

length(intersect(top_names$playerID, awards_2016$playerID))

intersect(top_names$playerID, awards_2016$playerID)
length(intersect(top_names$playerID, awards_2016$playerID))



setdiff(awards_2016$playerID, top_names$playerID)
length(setdiff(awards_2016$playerID, top_names$playerID))




library(rvest)

# Página
url <- "https://web.archive.org/web/20181024123133/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

# Todas as tabelas HTML
nodes <- html_nodes(h, "table")

# Examinar as 4 primeiras
for (i in 1:4) {
  cat("\n--- Table", i, "---\n")
  print(html_table(nodes[[i]], fill = TRUE))
}

# Extrai e limpa tab_1
tab_1 <- html_table(nodes[[10]], fill = TRUE)
tab_1 <- tab_1[-1, -1]  # remove a primeira linha (header real) e a coluna "No"
colnames(tab_1) <- c("Team", "Payroll", "Average")

# Extrai e limpa tab_2
tab_2 <- html_table(nodes[[19]], fill = TRUE)
tab_2 <- tab_2[-1, ]  # remove a primeira linha
colnames(tab_2) <- c("Team", "Payroll", "Average")

# Junta as tabelas
combined <- full_join(tab_1, tab_2, by = "Team")

# Ver quantas linhas tem
nrow(combined)



nrow(combined)


library(rvest)
library(tidyverse)

# URL da página do referendo Brexit
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

# Lê a página
page <- read_html(url)

# 1. Todas as tabelas
tab_all <- html_nodes(page, "table")
length(tab_all)  # Você já viu: 33

# 2. Apenas tabelas com classe 'wikitable'
tab_wiki <- html_nodes(page, "table.wikitable")
length(tab_wiki)  # Tente usar este número na plataforma

# 3. Inspecionar cada tabela visualmente (opcional)
# lapply(tab_wiki, html_table)

# 4. Mostrar os índices e títulos das tabelas (pra ajudar a contar visualmente, se necessário)
suppressWarnings({
  for (i in seq_along(tab_wiki)) {
    cat(paste0("\n--- Table ", i, " ---\n"))
    print(html_table(tab_wiki[[i]], fill = TRUE) %>% head(2))
  }
})

length(tab_wiki)



library(rvest)
library(tidyverse)

# URL da página da Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

# Lê o HTML da página
page <- read_html(url)

# Pega todos os elementos com tag <table>
tab <- html_nodes(page, "table")

# Conta quantas tabelas há
length(tab)


library(rvest)
library(tidyverse)

# URL da página
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
page <- read_html(url)

# Pega todas as tabelas
tabs <- html_nodes(page, "table")

# Loop para encontrar a tabela com 9 colunas e a primeira coluna chamada "Date(s) conducted"
for (i in seq_along(tabs)) {
  tbl <- html_table(tabs[[i]], fill = TRUE)
  
  if (ncol(tbl) == 9 && colnames(tbl)[1] == "Date(s) conducted") {
    cat("tabela número:", i, "tem 9 colunas e a primeira é 'Date(s) conducted'\n")
    print(head(tbl, 2))  # opcional, só pra você ver a amostra
    break  # para no primeiro match
  }
}


cat("tabela número:", i)



library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)


colnames(polls) <- c("dates", "remain", "leave", "undecided", "lead", 
                     "samplesize", "pollster", "poll_type", "notes")


polls <- polls %>% filter(str_detect(remain, "%"))

nrow(polls)



library(dslabs)
data(brexit_polls)

sum(month(brexit_polls$startdate) == 4)




library(lubridate)

sum(round_date(brexit_polls$enddate, unit = "week") == as.Date("2016-06-12"))














