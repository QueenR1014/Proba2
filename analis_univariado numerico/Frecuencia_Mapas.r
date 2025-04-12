# Leer archivo desde un subdirectorio
draft <- read.csv("vct_2024/matches/draft_phase.csv", stringsAsFactors = FALSE)

pick_map <- subset(draft, Tournament == "Valorant Champions 2024" & Action != "ban", select = c(Action, Map))

conteo_mapas <- table(pick_map$Map)

barplot(conteo_mapas,
        main = "Frecuencia de Mapas (Valorant Champions 2024)",
        xlab = "Mapa",
        ylab = "Frecuencia",
        col = "#9c1a87")
