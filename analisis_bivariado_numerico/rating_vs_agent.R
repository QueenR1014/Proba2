library(dplyr)
library(tidyr)
library(ggplot2)

# Leer y filtrar la base
df <- read.csv("vct_2024/matches/overview.csv")
df <- df %>%
  filter(
    Tournament == "Valorant Champions 2024",
    Side == "both",
    Map != "All Maps"
  )

# Paso 1: Rating promedio por jugador
df_rating_promedio <- df %>%
  group_by(Player) %>%
  summarise(Rating_Promedio = mean(Rating, na.rm = TRUE)) %>%
  arrange(desc(Rating_Promedio))

# Paso 2: Top 10 jugadores
top_10_jugadores_df <- df_rating_promedio %>%
  slice_head(n = 10)

# Paso 3: Filtrar registros de esos jugadores
df_top_jugadores <- df %>%
  inner_join(top_10_jugadores_df, by = "Player")

# Paso 4: Promedios por jugador y agente
df_agente_jugador <- df_top_jugadores %>%
  group_by(Player, Agents) %>%
  summarise(
    ACS_Promedio = mean(Average.Combat.Score, na.rm = TRUE),
    Rating_Promedio = mean(Rating, na.rm = TRUE),
    .groups = "drop"
  )

# Paso 5: Completar combinaciones faltantes
df_agente_jugador <- df_agente_jugador %>%
  complete(Player, Agents)

# Paso 6: Crear gráfico
ggplot(df_agente_jugador, aes(x = Player, y = Agents, fill = Rating_Promedio)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = ifelse(is.na(ACS_Promedio), "N/A", round(ACS_Promedio, 1))),
    size = 3,
    color = ifelse(is.na(df_agente_jugador$ACS_Promedio), "gray40", "black")
  ) +
  scale_fill_gradient(low = "#d891ef", high = "#6d33b4", na.value = "gray90") +
  labs(
    title = "Agente usado por jugador con promedio de Rating y ACS",
    x = "10 Mejores Jugadores",
    y = "Agente",
    fill = "Rating Promedio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
