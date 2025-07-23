# analise_exploratoria.R

library(tidyverse)
library(lubridate)

glimpse(vendas)

# Converter data para Date
vendas <- vendas %>%
  mutate(data = as.Date(data),
         mes = floor_date(data, "month"))

#Resumo geral

summary(vendas)

# Receita total por estado
receita_estado <- vendas %>%
  group_by(estado) %>%
  summarise(receita_total = sum(receita)) %>%
  arrange(desc(receita_total))

# Receita total por categoria
receita_categoria <- vendas %>%
  group_by(categoria) %>%
  summarise(receita_total = sum(receita)) %>%
  arrange(desc(receita_total))

# Receita mensal total
receita_mensal <- vendas %>%
  group_by(mes) %>%
  summarise(receita_total = sum(receita))

# Formas de pagamento usadas
pagamento_freq <- vendas %>%
  count(forma_pagamento) %>%
  arrange(desc(n))

# Gráficos
library(ggplot2)

#Construção de paletas 

# Paleta de cores sóbria
paleta <- c(
  "Eletrônicos" = "#4E79A7",
  "Roupas" = "#A0CBE8",
  "Acessórios" = "#F28E2B",
  "Casa" = "#59A14F",
  "Beleza" = "#8CD17D"
)

#Paleta formas de pagamento 

paleta_pagamento <- c(
  "Cartão de crédito" = "#4E79A7",
  "Débito" = "#A0CBE8",
  "Pix" = "#F28E2B",
  "Boleto" = "#59a14f" 
)

# Receita por estado
 ggplot(receita_estado, aes(x = reorder(estado, -receita_total), y = receita_total, fill = estado)) +
  geom_col(fill = "#4E79A7") +
  labs(title = "Receita total por estado", x = "Estado", y = "Receita (R$)") +
  theme_minimal()

# Receita por categoria
ggplot(receita_categoria, aes(x = reorder(categoria, -receita_total), y = receita_total, fill = categoria)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = paleta) +
  labs(title = "Receita total por categoria", x = "Categoria", y = "Receita (R$)") +
  theme_minimal()

# Receita mensal
 ggplot(receita_mensal, aes(x = mes, y = receita_total)) +
  geom_line(color = "#2C3E50", size = 1) +
  geom_point(color = "#2980B9") +
  labs(title = "Receita mensal total", x = "Mês", y = "Receita (R$)") +
  theme_minimal()

# Frequência de formas de pagamento
 ggplot(pagamento_freq, aes(x = reorder(forma_pagamento, -n), y = n, fill = forma_pagamento)) +
   geom_col(show.legend = FALSE) +
   scale_fill_manual(values= paleta_pagamento) +
  labs(title = "Formas de pagamento mais usadas", x = "Forma de pagamento", y = "Frequência") +
  theme_minimal()


