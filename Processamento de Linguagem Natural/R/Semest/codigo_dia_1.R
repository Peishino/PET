library(stringr)
library(tidytext)
library(dplyr)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(textdata)

dados <- read.csv("toy_story_scripts_by_act.csv", encoding = "UTF-8")

# Filme em um único texto
filme_1 <- dados %>%
  filter(Nome_do_filme == "Toy Story 1")

filme_1 <- paste(filme_1_df$Script, collapse = " ")

# Data frame
filme_1_df <- data.frame(text = filme_1)

# Tokenização
filme_1_tokens <- filme_1_df %>%
  unnest_tokens(word, text)

# Remover stop words
stop_words <- get_stopwords(language = "pt")

filme_1_tokens <- filme_1_tokens %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) > 2)

# Contagem de palavras
filme_1_word_count <- filme_1_tokens %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n))

# Visualização - Gráfico de Barras
top_words <- filme_1_word_count %>%
  top_n(10)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Palavras em Toy Story 1",
       x = "Palavras",
       y = "Frequência") +
  theme_minimal()

# Visualização - Word Cloud
wordcloud(words = filme_1_word_count$word,
          freq = filme_1_word_count$n,
          max.words = 20,
          colors = brewer.pal(8, "Dark2"))          

