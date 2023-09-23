# 0. Setup inicial ##############
## Carrega as bibliotecas
library(tidyverse)


# 1. Frequencia de posts por dia ##############

## Converta a coluna 'data' do dataframe postagens para o formato de data
postagens$data <- as.Date(postagens$data, format="%Y-%m-%d %H:%M:%S")

## Crie um gráfico de frequência de postagens por dia
ggplot(postagens) +
  geom_histogram(aes(x=data), bins=50, color="white", fill="blue") +
  labs(title="Frequência de Postagens por Dia", x="", y="Número de Postagens") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. Posts mais curtidos ##############

# Ordenando os dados por curtidas e selecionando as 10 postagens mais curtidas
top_posts <- postagens %>%
  arrange(desc(likes)) %>%
  head(10)

# Removendo postagens com texto NA
postagens <- postagens[!is.na(postagens$texto), ]

# Ordenando os dados por curtidas e selecionando as 10 postagens mais curtidas
top_posts <- postagens %>%
  arrange(desc(likes)) %>%
  head(10)

# Abreviando os textos das postagens
top_posts$texto_abreviado <- substr(top_posts$texto, 1, 50)

# Criando o gráfico
ggplot(top_posts, aes(x=reorder(texto_abreviado, -likes), y=likes)) +
  geom_bar(stat="identity", fill="skyblue") +
  coord_flip() +
  labs(title="Posts Mais Curtidos",
       y="Número de Curtidas",
       x="Posts") +
  theme_minimal()
