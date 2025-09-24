#install.packages("googlesheets4")
library(googlesheets4)

url <- "https://docs.google.com/spreadsheets/d/1ADpRfqFmci-P-psHRDWjYookIxx2_O-wczSQbAag9CM/edit?usp=sharing"
gs4_auth(email = "gabriel.moreira419@gmail.com", scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
medidas <- read_sheet(url, sheet = "Medidas")
treinos <- read_sheet(url, sheet = "Treinos")
sono <- read_sheet(url, sheet = "Sono")
peso <- read_sheet(url, sheet = "Peso")
setwd("C:/Users/bielm/OneDrive/Documents/GitHub/PET/eustat/Literalmente Eustatística")

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
    Soneca = duration(
      hour = hour(Soneca),
      minute = minute(Soneca),
      second = second(Soneca)
    ),
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
maximo_tempo_dormido <- seconds_to_period(max(sono$`Tempo dormido`, na.rm = TRUE))
media_tempo_dormido <- seconds_to_period(mean(sono$`Tempo dormido`, na.rm = TRUE))

jpeg("histograma_sono.jpg", width = 1200, height = 600)
hist(sono$`Tempo dormido`, main = "", xlab = "", col = "lightblue", border = "black")
dev.off()
jpeg("boxplot_sono.jpg", width = 1200, height = 600)
boxplot(sono$`Tempo dormido`, main = "", ylab = "", col = "lightgreen")
dev.off()

# Analisando por mês
sono <- sono %>%
  mutate(Mês = month(Data, label = TRUE, abbr = TRUE))

sono_mes <- sono %>%
  group_by(Mês) %>%
  summarise(
    Min_Tempo_Dormido = seconds_to_period(min(`Tempo dormido`, na.rm = TRUE)),
    Max_Tempo_Dormido = seconds_to_period(max(`Tempo dormido`, na.rm = TRUE)),
    Media_Tempo_Dormido = seconds_to_period(mean(`Tempo dormido`, na.rm = TRUE)),
    Soma_Tempo_Dormido = seconds_to_period(sum(`Tempo dormido`, na.rm = TRUE))
  )
media_dias_dormidos_mes <- seconds_to_period(mean(as.numeric(sono_mes$Soma_Tempo_Dormido)))
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


# gráfico de barras com uma linha de média 
library(ggplot2)
graf1 <- ggplot(sono_mes, aes(x = Mês, y = as.numeric(Soma_Tempo_Dormido))) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_hline(yintercept = as.numeric(media_dias_dormidos_mes), color = "#002B36", linetype = "dashed", size = 1) +
  geom_text(aes(label = as.character(Soma_Tempo_Dormido)), vjust = 1.5) +
  geom_text(
    x = tail(sono_mes$Mês, 1), 
    y = as.numeric(media_dias_dormidos_mes), 
    label = as.character(round(media_dias_dormidos_mes, 0)),
    color = "#002B36", # A cor do texto
    hjust = 0.33, # Ajuste horizontal para mover o texto para fora da barra
    vjust = -0.5 # Ajuste vertical para mover o texto para cima da linha
  ) +
  labs(title = "") + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )
graf1
ggsave(
  filename = "grafico_tempo_dormido_mes.jpg",
  plot = graf1,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)
getwd()
# Analisando por dia da semana

sono_semana <- sono %>%
  group_by(Dia_semana, Dia_semana_num) %>%
  summarise(
    Min_Tempo_Dormido = seconds_to_period(min(`Tempo dormido`, na.rm = TRUE)),
    Max_Tempo_Dormido = seconds_to_period(max(`Tempo dormido`, na.rm = TRUE)),
    Media_Tempo_Dormido = seconds_to_period(mean(`Tempo dormido`, na.rm = TRUE))
  ) %>%
  arrange(Dia_semana_num)

sono_semana_ordenado <- sono_semana %>%
  mutate(Dia_semana = factor(Dia_semana,
                             levels = c("domingo", "segunda-feira", "terça-feira",
                                        "quarta-feira", "quinta-feira",
                                        "sexta-feira", "sábado"))) %>%
  arrange(Dia_semana_num)


cores <- if_else(sono_semana_ordenado$Dia_semana %in% c("sábado", "domingo"), "lightcoral", "lightgray")
cores2 <- if_else(sono_semana_ordenado$Dia_semana %in% c("sábado", "domingo"), "lightgreen", "lightgray")

graf2 <- ggplot(sono_semana_ordenado, aes(x = Dia_semana, y = as.numeric(Min_Tempo_Dormido))) +
  geom_bar(stat = "identity", fill = cores) +
  geom_text(aes(label = as.character(Min_Tempo_Dormido)), vjust = -0.5) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )
ggsave(
  filename = "menor_sono_diasemana.jpg",
  plot = graf2,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

graf3 <- ggplot(sono_semana_ordenado, aes(x = Dia_semana, y = as.numeric(Max_Tempo_Dormido))) +
  geom_bar(stat = "identity", fill = cores2) +
  geom_text(aes(label = as.character(Max_Tempo_Dormido)), vjust = -0.5) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )
ggsave(
  filename = "maior_sono_diasemana.jpg",
  plot = graf3,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# Relacionando o tempo dormido com a soneca

soneca <- sono %>%
  filter(Soneca > duration(0))

minimo_soneca <- seconds_to_period(min(soneca$Soneca, na.rm = TRUE))
maximo_soneca <- seconds_to_period(max(soneca$Soneca, na.rm = TRUE))
media_soneca <- seconds_to_period(mean(soneca$Soneca, na.rm = TRUE))

media_soneca_mes <- soneca %>%
  group_by(Mês) %>%
  summarise(Media_Soneca = seconds_to_period(mean(Soneca, na.rm = TRUE)))
# histograma da soneca
seconds_to_period(4000)
jpeg("histograma_soneca.jpg", width = 1200, height = 600)
hist(soneca$Soneca, main = "", xlab = "", col = "lightblue", border = "black")
dev.off()
# mediana
mediana_soneca <- seconds_to_period(median(soneca$Soneca, na.rm = TRUE))

# qual a correlação?
correlacao <- cor(as.numeric(soneca$`Tempo dormido`), as.numeric(soneca$Soneca), use = "complete.obs")
correlacao 

cores_soneca <- ifelse(soneca$Soneca < duration(minutes = 40) | soneca$Soneca > duration(minutes = 250), "lightgray", "#002B36")

# eu tenho costume de tirar uma soneca de aproximadamente 40 minutos por dia, vou remover isso e um outlier pra ver como fica a correlação



soneca$cor <- ifelse(as.numeric(soneca$Soneca, "minutes") < 50 | as.numeric(soneca$Soneca, "minutes") > 250, "grey", "#002B36")

# Gera o gráfico 1
plot1 <- ggplot(soneca, aes(x = as.numeric(`Tempo dormido`), y = as.numeric(Soneca))) +
  geom_point(aes(color = cor), size = 3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "lightcoral") +
  labs(title = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#002B36" = "#002B36", "grey" = "grey"))

plot1
ggsave(
  filename = "soneca_tempo_dormido.jpg",
  plot = plot1,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

soneca_filtrada <- soneca %>%
  filter(as.numeric(Soneca, "minutes") >= 50 & as.numeric(Soneca, "minutes") <= 250)
correlacao_filtrada <- cor(as.numeric(soneca_filtrada$`Tempo dormido`), as.numeric(soneca_filtrada$Soneca), use = "complete.obs")
correlacao_filtrada
# Gera o gráfico 2
plot2 <- ggplot(soneca_filtrada, aes(x = as.numeric(`Tempo dormido`), y = as.numeric(Soneca))) +
  geom_point(color = "#002B36", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "lightcoral") +
  labs(title = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

plot2
ggsave(
  filename = "soneca_tempo_dormido_filtrado.jpg",
  plot = plot2,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)
