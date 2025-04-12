df <- read.csv("vct_2024/players_stats/players_stats.csv")

df = subset(df, Tournament == "Valorant Champions 2024")
df = subset(df, Stage == "All Stages")

library(dplyr)
df_promedios <- df %>%
  group_by(Player, Teams) %>%
  summarise(Rating = mean(Rating, na.rm = TRUE)) %>%
  ungroup()


library(plotly)
library(RColorBrewer)

# Obtener todos los equipos únicos
equipos <- unique(df_promedios$Teams)

# Asignar colores automáticamente (usando una paleta que proporciona buen contraste)
# Aseguramos que los colores sean lo suficientemente distintos para todos los equipos
colores <- RColorBrewer::brewer.pal(min(length(equipos), 12), "Set3")

# Si hay más de 12 equipos, utilizamos la paleta 'rainbow' para asegurar suficiente variedad
if (length(equipos) > 12) {
  colores <- grDevices::rainbow(length(equipos))
}

# Crear un vector con nombre -> color
color_equipo <- setNames(colores, equipos)

# Crear el gráfico interactivo con los colores actualizados
plot_ly(
  data = df_promedios,
  x = ~Rating,
  y = rep(0, nrow(df_promedios)),  # Todos los puntos alineados en y = 0
  type = 'scatter',
  mode = 'markers',
  text = ~paste("Jugador:", Player, "<br>Equipo:", Teams, "<br>Rating:", round(Rating, 2)),
  hoverinfo = 'text',
  marker = list(
    size = 8,
    color = ~color_equipo[Teams],  # Colores por equipo
    showscale = FALSE  # No mostrar la escala de colores en el gráfico
  ),
  color = ~Teams  # Agregar leyenda por equipo
) %>%
  layout(
    title = "Rating promedio por jugador (coloreado por equipo)",
    xaxis = list(title = "Rating Promedio", showgrid = FALSE),
    yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
    showlegend = TRUE,  # Mostrar la leyenda
    legend = list(
      title = "Equipos",
      traceorder = "normal",  # Orden normal de los elementos en la leyenda
      bgcolor = "rgba(255, 255, 255, 0.8)",  # Fondo blanco para la leyenda
      bordercolor = "rgba(0, 0, 0, 0.1)",  # Color de borde
      borderwidth = 1  # Ancho de borde de la leyenda
    )
  )

  
