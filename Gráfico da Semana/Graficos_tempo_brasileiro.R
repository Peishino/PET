vida_adulta <- 76 - 18
tempo_ocupado<- c(6.2,3.9,17.5,3.8,2.7,2.2,13.1,2.1,1.5,1.1)
atividade<- c("Trabalho Doméstico","Cuidados Familiares","Sono","Deslocamento","Cuidadeos Pessoais","Educação","Trabalho Remunerado","Alimentação","Outro Trabalho (Não remunerado)", "Gestão Financeira")
percent <- tempo_ocupado/vida_adulta*100
library(ggplot2)
library(dplyr)
library(ggtext)
# gráfico de barras com rótulos
df <- data.frame(atividade, tempo_ocupado, percent)
df <- df %>%
  mutate(
    atividade = factor(atividade, levels = atividade[order(tempo_ocupado)]),
    label_percent = paste0(round(percent, 1), "%")  # rótulos formatados
  )

ggplot(df, aes(x = atividade, y = tempo_ocupado)) +
  geom_bar(stat = "identity", fill = "#172C3D") +
  geom_text(aes(label = label_percent), hjust = -0.1) +  # adiciona os percentuais
  coord_flip() +
  labs(title = "Quanto tempo passamos ocupados na vida adulta?",
       x = "",
       y = "Anos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = -5.0, size = 18),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  ylim(0, max(df$tempo_ocupado) + 2)  # espaço para os rótulos acima das barras
  
  
  
tempo_livre <- c(3.6,2.9,2.2,1.2,0.8,2.3,1.5,1.3,1.1,1,1,0.7)
atividade_livre <- c("Uso do Celular","Tv e Rádio","Esporte e Bem-Estar","Atividade Religiosa","Participar de Eventos","Descanso e Ócio","Leituras","Atividade Artística","Preparar Receitas","Bar e Restaurantes","Atividade ao ar livre","Shopping e Lojas")
percent_livre <- tempo_livre/vida_adulta*100
df_livre <- data.frame(atividade_livre, tempo_livre, percent_livre)
df_livre <- df_livre %>%
  mutate(
    atividade_livre = factor(atividade_livre, levels = atividade_livre[order(tempo_livre)]),
    label_percent = paste0(round(percent_livre, 1), "%")  # rótulos formatados
  )
ggplot(df_livre, aes(x = atividade_livre, y = tempo_livre)) +
  geom_bar(stat = "identity", fill = "#172C3D") +
  geom_text(aes(label = label_percent), hjust = -0.1) +  # adiciona os percentuais
  coord_flip() +
  labs(title = "Como os adultos usam o tempo livre?",
       x = "",
       y = "Anos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = -0.65, size = 18),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
15599/360
43/vida_adulta
