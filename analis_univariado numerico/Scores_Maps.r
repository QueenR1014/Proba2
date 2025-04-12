sc <- read.csv("vct_2024/matches/maps_scores.csv", stringsAsFactors = FALSE)

data <- subset(
    sc, 
    Tournament == "Valorant Champions 2024", 
    select = c(Team.A.Score, Team.B.Score)
    )

data$Resultado <- paste(data$Team.A.Score, data$Team.B.Score, sep = "-")
frecuencias <- table(data$Resultado)
frecuencias_ordenadas <- sort(frecuencias, decreasing = TRUE)

ruta_jpg <- "graficos/frecuencia_resultados.jpg"

jpeg(ruta_jpg, width = 800, height = 600, quality = 100)

barplot(frecuencias_ordenadas,
        las = 2,
        col = "#962626",
        main = "Frecuencia de Resultados por Marcador",
        xlab = "Marcador (A-B)",
        ylab = "Frecuencia")

dev.off()
