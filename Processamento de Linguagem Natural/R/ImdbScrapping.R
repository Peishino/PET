library(rvest)
library(tidyverse)
library(stringr)

url <- "https://www.imdb.com/"

html <- read_html(url)


top250 <- html %>%
  html_elements(".navlinkcat__listContainerInner")%>%
  html_elements("a")%>%
  html_attr("href")


top250 <- top250[2]
top250 <- str_c("https://www.imdb.com",top250, sep = "") # capitulo 11 pag 197
top250 <- read_html(top250)

top250movies <- top250 %>%
  html_elements("ul") %>%
  html_elements("h3") %>%
  html_text2()

'Expressão regular:
^ = inicio da linha
\\d = um digito
\\d+ = sequência de digitos
\\. = ponto
\\s = espaço
\\s+ = sequência de espaços
'
top250movies <- str_remove(top250movies,"^\\d+\\.\\s+")

metadata <- top250 %>%
  html_elements("ul") %>% 
  html_elements(".cli-title-metadata-item")%>%
  html_text2()


rating <- top250 %>%
  html_elements("ul") %>%
  html_elements(".ratingGroup--imdb-rating")%>%
  html_text2()
  

'Expressão regular:
\\( = parêntese de abertura
\\[^)*] = qualquer coisa que não for um ")" podendo ter 0 ou mais caracteres
\\) = parêntese de fechamento
'
rating <- str_remove(rating, "\\([^)]*\\)")


# montando o dataframe

df <- data.frame(
  Filme = top250movies,
  Avaliacao = rating,
  stringsAsFactors = FALSE
)
df$Ano <- metadata[seq(1, length(metadata), by = 3)]
df$Duracao <- metadata[seq(2, length(metadata), by = 3)]
df$IdadePermitida <- metadata[seq(3, length(metadata), by = 3)]

# mudando o tempo para minutos
tempo <- str_extract_all(df$Duracao,"\\d+")
DuracaoMin <- c()
for(i in 1:length(tempo)){
  numeros <- as.numeric(tempo[[i]])
  horas <- numeros[1]
  minutos <- ifelse(length(numeros) > 1, numeros[2], 0) # alguns filmes não tinham duração em minutos
  DuracaoMin <- c(DuracaoMin,horas * 60 + minutos)
}

df$Duracao <- DuracaoMin

