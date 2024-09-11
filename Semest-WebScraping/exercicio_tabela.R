url <- "https://www.nsctotal.com.br/noticias/20-melhores-filmes-todos-os-tempos-ate-2020"

html <- read_html(url)
html

html_elements(html,"h2")

titulos <-html |>
  html_elements("h2.wp-block-heading")|>
  html_text2()

titulos_1 <- titulos[2:21]
titulos_1

titulos_limpo <- titulos_1 |>
  str_remove("^\\d+\\s.\\s") 



url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o#:~:text=A%20unidade%20federativa%20com%20o,Para%C3%ADba%20(86%2C8%25)"

html<- read_html(url)
html

tabela<- html |>
  html_elements("table.wikitable")|>
  html_table()
tabela2<- tabela[[2]]

