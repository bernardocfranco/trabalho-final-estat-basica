# Importar pacotes mínimos

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

# Ler base de dados do clone criado no repositório (pasta '/dados')

df <- read_xlsx("dados/BASE_cargos2.xlsx", "BASE_cargos", na = "NA")

codebook <- read_xlsx("dados/BASE_cargos2.xlsx", "Codebook")

# Revelar o número de linhas e colunas do df

c(linhas = nrow(df), colunas = ncol(df))

# Mostrar o nome das colunas do df

names(df)

# Ler o número de observações por 'orgao_sup' em df

df |>
  dplyr::count(orgao_sup, name = "n")

# Contar o número de valores ausentes em variáveis relevantes em df

df |>
  summarise(across(c(nivel, instr, area, inst_ensino, inst_origem), ~ sum(is.na(.)), .names = "na_{col}"))

# Checar valores observados em 'exp_adm', 'exp_car', 'nivel', 'instr' e 'indicacao' em df

df |>
  group_by(exp_adm) |>
  summarize(n = n(), .groups = "drop")

df |>
  group_by(exp_car) |>
  summarize(n = n(), .groups = "drop")

df |>
  group_by(nivel) |>
  summarize(n = n(), .groups = "drop")

df |>
  group_by(instr) |>
  summarize(n = n(), .groups = "drop")

df |>
  group_by(indicacao) |>
  summarize(n = n(), .groups = "drop")

# Corrigir valores inadequados na variável 'exp_adm' em df

df <- df |>
  mutate(
    exp_adm = str_remove_all(as.character(exp_adm), "`"),
    exp_adm = case_when(
      exp_adm == "3" ~ NA_real_,
      TRUE ~ as.numeric(exp_adm)
    )
  )

## Validar correção de valores inadequados na variável 'exp_adm' em df

df |>
  group_by(exp_adm) |>
  summarize(n = n(), .groups = "drop")

# Corrigir valores inadequados na variável 'exp_car' em df

df <- df |>
  mutate(
    exp_car_imputado = is.na(exp_car),
    exp_car = case_when(
      is.na(exp_car) ~ 0,
      exp_car == 2   ~ 2,
      exp_car == 3   ~ 2,
      exp_car == 5   ~ 3,
      TRUE ~ exp_car
    )
  )

## Validar correção de valores inadequados na variável 'exp_car' em df

df |>
  group_by(exp_car) |>
  summarize(n = n(), .groups = "drop")

# Corrigir valores inadequados na variável 'instr' em df

df <- df |>
  mutate(
    instr = case_when(
      instr == "graduacao" ~ "superior",
      TRUE ~ instr
    )
  )

## Validar correção de valores inadequados na variável 'inst' em df

df |>
  group_by(instr) |>
  summarize(n = n(), .groups = "drop")

# Corrigir valores inadequados na variável 'indicacao' em df

limites <- tibble::tibble(
  indicacao = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  inicio    = as.Date(c("2003-01-01", "2011-01-01", "2016-05-12", "2019-01-01")),
  fim       = as.Date(c("2010-12-31", "2016-05-11", "2018-12-31", "2022-12-31"))
)

df <- df |>
  mutate(
    entrada = as.Date(entrada),
    indicacao_corrigida = case_when(
      entrada <= as.Date("2010-12-31") ~ "Lula",
      entrada <= as.Date("2016-05-11") ~ "Rousseff",
      entrada <= as.Date("2018-12-31") ~ "Temer",
      entrada <= as.Date("2022-12-31") ~ "Bolsonaro",
      TRUE ~ NA_character_
    ),
    indicacao_divergente = indicacao != indicacao_corrigida
  )

## Validar correção de valores inadequados na variável 'indicacao' em df

df |>
  group_by(indicacao) |>
  summarize(n = n(), .groups = "drop")

# Filtrar a base de dados para exibir apenas valores 'minc' e 'mapa' na variável 'orgao_sup' em df

df <- df |>
  filter(orgao_sup != "mcti")

# Exibir o número de observações válidas por pasta ('minc' e 'mapa') em df

df |>
  group_by(orgao_sup) |>
  summarize(n = n(), .groups = "drop")

# Exibir o número de nomeações com experiência administrativa (exp_adm = 1) em df

df |>
  filter(exp_adm == 1) |>
  summarize(sum(exp_adm))

# Exibir a proporção de nomeações com experiência administrativa (exp_adm = 1) em df

df |>
  summarize(prop = sum(exp_adm == 1, na.rm = TRUE) / sum(!is.na(exp_adm)))

# Calcular intervalo de confiança de 95% para a proporção acima, bem como a margem de erro associada

k <- sum(df$exp_adm == 1, na.rm = TRUE)

n <- sum(!is.na(df$exp_adm))

res <- prop.test(k, n, correct = FALSE)

ci <- res$conf.int

me <- (ci[2] - ci[1]) / 2

p_hat <- mean(ci)

data.frame(p_hat = p_hat, lower = ci[1], upper = ci[2], margin_of_error = me)

# Realizar o primeiro teste de hipótese bivariado

## Obter k e n por pasta

tab <- df |>
  group_by(orgao_sup) |>
  summarize(
    k = sum(exp_adm == 1, na.rm = TRUE),
    n = sum(!is.na(exp_adm)),
    p_hat = k / n,
    .groups = "drop"
  )

k1 <- tab$k[tab$orgao_sup == "minc"]
n1 <- tab$n[tab$orgao_sup == "minc"]
p1 <- tab$p_hat[tab$orgao_sup == "minc"]

k2 <- tab$k[tab$orgao_sup == "mapa"]
n2 <- tab$n[tab$orgao_sup == "mapa"]
p2 <- tab$p_hat[tab$orgao_sup == "mapa"]

## Obter a estatística z

p_pool <- (k1 + k2) / (n1 + n2)

z <- (p1 - p2) / sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))

p_value <- 2 * (1 - pnorm(abs(z)))

## Calcular o intervalo de confiança para a diferença

zcrit <- qnorm(0.975)

se_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)

ci_lower <- (p1 - p2) - zcrit * se_diff

ci_upper <- (p1 - p2) + zcrit * se_diff

## Exibir os resultados da análise

data.frame(
  k1 = k1, n1 = n1, p1 = p1,
  k2 = k2, n2 = n2, p2 = p2,
  p_pool = p_pool, z = z, p_value = p_value,
  diff = p1 - p2, ci_lower = ci_lower, ci_upper = ci_upper
)

### Interpretação qualitativa: A proporção de nomeações com experiência foi de 88.1% no MINC e 92.0% no MAPA
    ### (diferença = −3.85 pp; IC 95% para a diferença: −7.94 pp a 0.24 pp). Pelo teste z (aproximação normal) a
    ### diferença não é estatisticamente significativa ao nível de 5% (z = −1.90; p = 0.057). Em termos práticos,
    ### os dados sugerem uma tendência de maior proporção no MAPA, mas a evidência estatística é insuficiente para
    ### concluir que as pastas diferem de forma robusta.

# Realizar o segundo teste de hipótese bivariado

## Criar a variável 'alto_nivel'

df <- df |>
  mutate(alto_nivel = case_when(
    nivel == 4 ~ 0,
    nivel == 5 ~ 1,
    nivel == 6 ~ 1,
    TRUE       ~ NA_real_
  ))

## Obter k e n por pasta

tab2 <- df |>
  group_by(orgao_sup) |>
  summarize(
    k = sum(alto_nivel == 1, na.rm = TRUE),
    n = sum(!is.na(alto_nivel)),
    p_hat = k / n,
    .groups = "drop"
  )

k1 <- tab2$k[tab2$orgao_sup == "minc"]
n1 <- tab2$n[tab2$orgao_sup == "minc"]
p1 <- tab2$p_hat[tab2$orgao_sup == "minc"]

k2 <- tab2$k[tab2$orgao_sup == "mapa"]
n2 <- tab2$n[tab2$orgao_sup == "mapa"]
p2 <- tab2$p_hat[tab2$orgao_sup == "mapa"]

## Calcular um IC de 95% para as proporções por pasta

res2_1 <- prop.test(k1, n1, correct = FALSE)

ci2_1 <- res2_1$conf.int

ci2_1

res2_2 <- prop.test(k2, n2, correct = FALSE)

ci2_2 <- res2_2$conf.int

ci2_2

## Realiza um teste de hipótese para a diferença de proporção entre as pastas

### Obter a estatística z

p_pool <- (k1 + k2) / (n1 + n2)

z <- (p1 - p2) / sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))

p_value <- 2 * (1 - pnorm(abs(z)))

### Calcular o intervalo de confiança para a diferença

zcrit <- qnorm(0.975)

se_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)

ci_lower <- (p1 - p2) - zcrit * se_diff

ci_upper <- (p1 - p2) + zcrit * se_diff

### Exibir os resultados da análise
898
data.frame(
  k1 = k1, n1 = n1, p1 = p1,
  k2 = k2, n2 = n2, p2 = p2,
  p_pool = p_pool, z = z, p_value = p_value,
  diff = p1 - p2, ci_lower = ci_lower, ci_upper = ci_upper
)

#### Interpretação qualitativa: A proporção de cargos de alto nível é de 42.5% no MINC e 35.6% no MAPA
      #### (diferença = +6.8 pp; IC 95% = 0.33 pp a 13.37 pp). Pelo teste z para diferença de proporções,
      #### essa diferença é estatisticamente significativa ao nível de 5% (z = 2.07; p = 0.0389). Em termos
      #### práticos, observa‑se uma vantagem moderada do MINC na proporção de nomeações para níveis 5–6.