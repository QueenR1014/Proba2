library(ggplot2)

agents <- read.csv("vct_2024/agents/teams_picked_agents.csv", stringsAsFactors = FALSE)

agents_vc2024 <- subset(agents, Tournament == "Valorant Champions 2024")


tabla_agente_mapa <- as.data.frame(table(agents_vc2024$Map, agents_vc2024$Agent))
colnames(tabla_agente_mapa) <- c("Mapa", "Agente", "Frecuencia")


ruta_jpg <- "graficos/agenteXmapa.jpg"

jpeg(ruta_jpg, width = 800, height = 600, quality = 100)

ggplot(tabla_agente_mapa, aes(x = Agente, y = Mapa, fill = Frecuencia)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#daecf3", high = "#03407c") +
    labs(title = "Frecuencia de Agentes por Mapa (Valorant Champions 2024)",
        x = "Agente", y = "Mapa") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()