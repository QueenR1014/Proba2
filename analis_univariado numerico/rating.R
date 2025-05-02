library(dplyr)
library(ggplot2)
library(RColorBrewer)


df <- read.csv("vct_2024/players_stats/players_stats.csv")
df <- subset(df, Tournament == "Valorant Champions 2024" & Stage == "All Stages")


df_promedios <- df %>%
  group_by(Player, Teams) %>%
  summarise(Rating = mean(Rating, na.rm = TRUE), .groups = "drop")


media <- mean(df_promedios$Rating, na.rm = TRUE)
mediana <- median(df_promedios$Rating, na.rm = TRUE)
sd_val <- sd(df_promedios$Rating, na.rm = TRUE)

lineas <- data.frame(
  xintercept = c(media, mediana, media + sd_val, media - sd_val),
  tipo = c("Media", "Mediana", "Media + SD", "Media - SD")
)


equipos <- unique(df_promedios$Teams)
colores <- if (length(equipos) > 12) rainbow(length(equipos)) else brewer.pal(length(equipos), "Set3")
color_equipo <- setNames(colores, equipos)


if (!dir.exists("graficos")) dir.create("graficos")


jpeg("graficos/rating_promedio.jpg", width = 1000, height = 500, quality = 100)

ggplot(df_promedios, aes(x = Rating, y = 0, color = Teams)) +
  geom_point(size = 3) +
  scale_color_manual(values = color_equipo) +
  geom_vline(data = lineas, aes(xintercept = xintercept, linetype = tipo), color = "black") +
  scale_linetype_manual(name = "Estadígrafos", values = c("dashed", "dashed", "dotted", "dotted")) +

  annotate("text", x = media, y = 0.1, label = sprintf("Media = %.2f", media), angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = mediana, y = 0.3, label = sprintf("Mediana = %.2f", mediana), angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = media + sd_val, y = 0.5, label = sprintf("+SD = %.2f", media + sd_val), angle = 90, vjust = -0.5, color = "black") +
  annotate("text", x = media - sd_val, y = 0.5, label = sprintf("-SD = %.2f", media - sd_val), angle = 90, vjust = -0.5, color = "black") +
  labs(title = "Rating promedio por jugador (coloreado por equipo)",
       x = "Rating Promedio", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

dev.off()
