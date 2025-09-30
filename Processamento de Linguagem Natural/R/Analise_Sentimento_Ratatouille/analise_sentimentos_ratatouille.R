# pacotes
library(pdftools)
library(stringr)
library(tidytext)
library(dplyr)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(textdata)

ratatouille_pages <- pdf_text("pdf_ratatouille.pdf")


cat(substr(ratatouille_pages[1], 1, 500))
# a primeira pagina n tem nada de interessante

cat(substr(ratatouille_pages[48], 1, 500))
# começo do script 

script <- ratatouille_pages[2:48]
ratatouille_text_unico <- str_c(script, collapse = "\n\n")

ratatouille_limpo <- str_replace_all(ratatouille_text_unico, "\\\\", "")
ratatouille_limpo <- str_replace_all(ratatouille_limpo, "Page \\d+/\\d+", "")
ratatouille_limpo <- str_replace_all(ratatouille_limpo, "\n{2,}", "\n")
ratatouille_limpo <- str_trim(ratatouille_limpo)
ratatouille_limpo <- str_replace_all(ratatouille_limpo, " {2,}", " ")

cat(substr(ratatouille_limpo, 1, 800))


ratatouille_df <- tibble(line = 1, text = ratatouille_limpo)
# tokenização
ratatouille_tokens <- ratatouille_df %>%
  unnest_tokens(word, text)

# removendo as stop words
data("stop_words")

ratatouille_tokens_limpo <- ratatouille_tokens %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) > 2)

# Mostrar as 15 palavras mais frequentes
palavras_mais_frequentes <- ratatouille_tokens_limpo %>%
  count(word, sort = TRUE) %>%
  top_n(15) 

print(palavras_mais_frequentes)

# grafico de barras 
palavras_mais_frequentes %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "As 15 Palavras Mais Frequentes no Roteiro de Ratatouille",
    x = "Palavra",
    y = "Frequência"
  ) +
  theme_minimal()


# nuvem de palavras
wordcloud(
  words = palavras_mais_frequentes$word,
  freq = palavras_mais_frequentes$n,
  max.words = 15,
  colors = brewer.pal(8,"OrRd")
)

# léxico Bing (polaridade: positivo/negativo)
bing_sentiment <- get_sentiments("bing")

ratatouille_sentimento <- ratatouille_tokens_limpo %>%
  inner_join(bing_sentiment, by = "word")
# uma mesma palavra tem 
head(ratatouille_sentimento)
table(ratatouille_sentimento$sentiment)


# frequência de sentimentos 
contagem_polaridade <- ratatouille_sentimento %>%
  count(sentiment, sort = TRUE)

# comparativo filme todo
contagem_polaridade %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(
    title = "Balanço de Polaridade (Positivo vs. Negativo)",
    x = "Polaridade",
    y = "Contagem de Palavras"
  ) +
  scale_fill_manual(values = c("negative" = "tomato", "positive" = "forestgreen")) +
  theme_minimal()


ratatouille_frequencias_polaridade <- ratatouille_sentimento %>%
  count(word, sentiment)

par(mfrow = c(1, 2)) 

#Nuvem de Palavras POSITIVAS
dados_positivos <- ratatouille_frequencias_polaridade %>%
  filter(sentiment == "positive")

wordcloud(
  words = dados_positivos$word,
  freq = dados_positivos$n,      
  max.words = 15,
  colors = "green"
)


#Nuvem de Palavras NEGATIVAS
dados_negativos <- ratatouille_frequencias_polaridade %>%
  filter(sentiment == "negative")

wordcloud(
  words = dados_negativos$word,
  freq = dados_negativos$n,      
  max.words = 15,
  colors = brewer.pal(8, "Reds")
)

par(mfrow = c(1, 1))


# léxico NSS (emoções)
nrc_sentimento <- get_sentiments("nrc")

ratatouille_emocoes <- ratatouille_tokens_limpo %>%
  inner_join(nrc_sentimento, by = "word") %>%
  # O léxico NSS também inclui 'positive' e 'negative'
  filter(!sentiment %in% c("positive", "negative"))

head(ratatouille_emocoes)

contagem_emocoes <- ratatouille_emocoes %>%
  count(sentiment, sort = TRUE)
# gráfico de emoções
contagem_emocoes %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(
    title = "Frequência das Emoções no Roteiro de Ratatouille (Léxico NSS)",
    x = "Emoção",
    y = "Contagem de Palavras"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 


# análise de emoção ao longo do filme
ratatouille_timed_tokens <- tibble(text = ratatouille_limpo) %>%
  unnest_tokens(word, text) %>%
  mutate(word_number = row_number())

num_capitulos <- 50

ratatouille_por_momento <- ratatouille_timed_tokens %>%
  mutate(chapter = ceiling(word_number / (n() / num_capitulos))) %>%
  anti_join(stop_words, by = "word")

bing_sentiment <- get_sentiments("bing")

ratatouille_sentimento_tempo <- ratatouille_por_momento %>%
  inner_join(bing_sentiment, by = "word") %>%
  
  count(chapter, sentiment) %>%
  
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  
  mutate(sentiment_score = positive - negative)


ratatouille_sentimento_tempo %>%
  ggplot(aes(x = chapter, y = sentiment_score, fill = sentiment_score > 0)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Arco Emocional do Roteiro de Ratatouille",
    subtitle = "Variação de Sentimento (Positivo - Negativo) ao Longo de 50 Momentos",
    x = "Momento no Filme (Capítulo de 1 a 50)",
    y = "Pontuação de Sentimento Líquida (Positivo - Negativo)"
  ) +
  scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "indianred")) +
  theme_minimal()

