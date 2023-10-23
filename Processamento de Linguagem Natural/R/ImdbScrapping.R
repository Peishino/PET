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


top250movies <- as.data.frame(top250movies)
