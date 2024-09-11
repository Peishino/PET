install.packages("dplyr")
install.packages("rvest")
install.packages("stringr")

library(rvest)
library(dplyr)
library(stringr)

url <- "https://www.nsctotal.com.br/noticias/20-melhores-filmes-todos-os-tempos-ate-2020"

html <- read_html(url)
html

html_elements(html,"h2")

titulos <-html |>
  html_elements("h2.wp-block-heading")|>
  html_text2()

titulos_1 <- titulos[2:21]
titulos_1

url <- "https://www.cnnbrasil.com.br/economia/"
html <- read_html(url)

html |>
  html_elements("h3.block__news__title") |>
  html_text2()
