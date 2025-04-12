draft <- read.csv("vct_2024/matches/draft_phase.csv", stringsAsFactors = FALSE)

pick_map <- subset(draft, Tournament == "Valorant Champions 2024" & Action != "ban", select = c(Action, Map))

conteo_mapas <- table(pick_map$Map)

ruta_jpg <- "graficos/frecuencia_mapas.jpg"

jpeg(ruta_jpg, width = 800, height = 600, quality = 100)

barplot(conteo_mapas,
        main = "Frecuencia de Mapas",
        xlab = "Mapa",
        ylab = "Frecuencia",
        col = "#852871",
        las = 2)

dev.off()