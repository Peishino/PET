install.packages("googlesheets4")
library(googlesheets4)

url <- ""
gs4_auth(email = "", scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
medidas <- read_sheet(url, sheet = "Medidas")
treinos <- read_sheet(url, sheet = "Treinos")
sono <- read_sheet(url, sheet = "Sono")
peso <- read_sheet(url, sheet = "Peso")


library(dplyr)
library(lubridate)

sono <- sono %>%
  mutate(
    `Tempo dormido` = hms(format(`Tempo dormido`, "%H:%M:%S")),
    Soneca = hms(format(Soneca, "%H:%M:%S")),
    Total_dormido = hms(format(Total_dormido, "%H:%M:%S"))
  )

sono <- sono %>%
  mutate(
    `Tempo dormido` = duration(
      hour = hour(`Tempo dormido`),
      minute = minute(`Tempo dormido`),
      second = second(`Tempo dormido`)
    ),
    Soneca = seconds_to_period(duration(
      hour = hour(Soneca),
      minute = minute(Soneca),
      second = second(Soneca)
    )),
    Total_dormido = duration(
      hour = hour(Total_dormido),
      minute = minute(Total_dormido),
      second = second(Total_dormido)
    )
  )

sono <- sono %>%
  filter(`Tempo dormido` > duration(0))

# Análise exploratória

# Mínimo e Máximo de Tempo Dormido Geral
minimo_tempo_dormido <- seconds_to_period(min(sono$`Tempo dormido`, na.rm = TRUE))
maximo_tempo_dormido <- seconds_to_period(max(sono_filtrado$`Tempo dormido`, na.rm = TRUE))
media_tempo_dormido <- seconds_to_period(mean(sono$`Tempo dormido`, na.rm = TRUE))

hist(sono$`Tempo dormido`, main = "", xlab = "", col = "lightblue", border = "black")
boxplot(sono$`Tempo dormido`, main = "", ylab = "", col = "lightgreen")

# Analisando por mês
sono <- sono %>%
  mutate(Mês = month(Data, label = TRUE, abbr = TRUE))

sono_mes <- sono %>%
  group_by(Mês) %>%
  summarise(
    Min_Tempo_Dormido = seconds_to_period(min(`Tempo dormido`, na.rm = TRUE)),
    Max_Tempo_Dormido = seconds_to_period(max(`Tempo dormido`, na.rm = TRUE)),
    Media_Tempo_Dormido = seconds_to_period(mean(`Tempo dormido`, na.rm = TRUE))
  )

par(mfrow = c(1,3))

barplot(as.numeric(sono_mes$Min_Tempo_Dormido),
        names.arg = sono_mes$Mês,
        main = "Mínimo Tempo Dormido por Mês",
        col = "lightcoral",
        ylab = "Tempo Dormido (em segundos)",
        ylim = c(0, 40000))

barplot(as.numeric(sono_mes$Media_Tempo_Dormido),
        names.arg = sono_mes$Mês,
        main = "Média Tempo Dormido por Mês",
        col = "lightgreen",
        ylab = "Tempo Dormido (em segundos)",
        ylim = c(0, 40000))

barplot(as.numeric(sono_mes$Max_Tempo_Dormido),
        names.arg = sono_mes$Mês,
        main = "Máximo Tempo Dormido por Mês",
        col = "lightblue",
        ylab = "Tempo Dormido (em segundos)",
        ylim = c(0, 40000))
# fazer soma do tempo dormido por mês
# ver se dá pra relacionar maior tempo dormido com as férias da faculdade

# Analisando por dia da semana

sono_semana <- sono %>%
  group_by(Dia_semana, Dia_semana_num) %>%
  summarise(
    Min_Tempo_Dormido = seconds_to_period(min(`Tempo dormido`, na.rm = TRUE)),
    Max_Tempo_Dormido = seconds_to_period(max(`Tempo dormido`, na.rm = TRUE)),
    Media_Tempo_Dormido = seconds_to_period(mean(`Tempo dormido`, na.rm = TRUE))
  ) %>%
  arrange(Dia_semana_num)

library(ggplot2)

cores <- if_else(sono_semana$Dia_semana %in% c("sábado", "domingo"), "lightcoral", "lightgray")

ggplot(sono_semana, aes(x = Dia_semana, y = as.numeric(Min_Tempo_Dormido))) +
  geom_bar(stat = "identity", fill = cores) +
  geom_text(aes(label = as.character(Min_Tempo_Dormido)), vjust = -0.5) +
  labs(title = "Mínimo Tempo Dormido por Dia da Semana") + 
  theme_minimal() +
  theme(axis.title.x = element_blank())

  



sono_semana_ordenado <- sono_semana %>%
  mutate(Dia_semana = factor(Dia_semana,
                             levels = c("domingo", "segunda-feira", "terça-feira",
                                        "quarta-feira", "quinta-feira",
                                        "sexta-feira", "sábado"))) %>%
  arrange(Dia_semana_num)


cores <- if_else(sono_semana_ordenado$Dia_semana %in% c("sábado", "domingo"), "lightcoral", "lightgray")
cores2 <- if_else(sono_semana_ordenado$Dia_semana %in% c("sábado", "domingo"), "lightgreen", "lightgray")

ggplot(sono_semana_ordenado, aes(x = Dia_semana, y = as.numeric(Min_Tempo_Dormido))) +
  geom_bar(stat = "identity", fill = cores) +
  geom_text(aes(label = as.character(Min_Tempo_Dormido)), vjust = -0.5) +
  # Personalizações do tema
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )


ggplot(sono_semana_ordenado, aes(x = Dia_semana, y = as.numeric(Max_Tempo_Dormido))) +
  geom_bar(stat = "identity", fill = cores2) +
  geom_text(aes(label = as.character(Max_Tempo_Dormido)), vjust = -0.5) +
  # Personalizações do tema
  theme_minimal() + # Já remove grande parte das linhas de fundo
  theme(
    # Remove as linhas de fundo (major e minor)
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove o título do eixo Y e os rótulos de valores
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # Remove o título do eixo X
    axis.title.x = element_blank()
  )
