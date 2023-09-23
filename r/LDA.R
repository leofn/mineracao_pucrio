# 0. Setup inicial ##############
## Carrega as bibliotecas
library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)

# Removendo postagens com texto NA

postagens <- postagens[!is.na(postagens$texto), ]

# Lista expandida de stop words em português
custom_stopwords <- c(stopwords("pt"), 
                      "é", "à", "ao", "aos", "com", "por", "para", "sem", "sob", 
                      "entre", "dentro", "fora", "através", "durante", "antes", "depois")

# Pré-processamento do texto
corpus <- Corpus(VectorSource(postagens$texto))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Criando a matriz documento-termo
dtm <- DocumentTermMatrix(corpus)

# Aplicando o modelo LDA
lda_model <- LDA(dtm, k = 3, control = list(seed = 1234))

# Visualizando os tópicos
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
