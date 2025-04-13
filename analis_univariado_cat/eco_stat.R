library(dplyr)
library(ggplot2)
library(tidyr)

# Leer archivo
df <- read.csv("vct_2024/matches/eco_stats.csv")

# Filtrar torneo y mapa
df <- subset(df, Tournament == "Valorant Champions 2024")
df_all_maps <- subset(df, Map == "All Maps")

# Eliminar "Pistol Won" del análisis
df_all_maps <- df_all_maps %>% filter(Type != "Pistol Won")

# Agrupar y resumir
# Sumar y preparar datos
df_sumado <- df_all_maps %>%
  group_by(Type) %>%
  summarise(
    Initiated = sum(Initiated, na.rm = TRUE),
    Won = sum(Won, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Not_Won = Initiated - Won
  )

# Preparar para el gráfico
df_plot <- df_sumado %>%
  select(Type, Won, Not_Won, Initiated) %>%
  pivot_longer(cols = c("Won", "Not_Won"), names_to = "Estado", values_to = "Cantidad") %>%
  mutate(
    Estado = recode(Estado, "Won" = "Ganadas", "Not_Won" = "No Ganadas"),
    Porcentaje = round((Cantidad / Initiated) * 100, 1),
    Etiqueta = paste0(Porcentaje, "% (", Cantidad, ")")
  )

# Gráfico
ggplot(df_plot, aes(x = Type, y = Cantidad, fill = Estado)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = Etiqueta),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  # Etiqueta total de Initiated arriba
  geom_text(
    data = df_sumado,
    aes(x = Type, y = Initiated, label = Initiated),
    inherit.aes = FALSE,
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Rondas Iniciadas vs Ganadas por Tipo",
    x = "Tipo de Ronda",
    y = "Cantidad Total",
    fill = "Estado"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No Ganadas" = "#ff4654", "Ganadas" = "#6968d5")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))