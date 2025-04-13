library(dplyr)
library(ggplot2)

ov <- read.csv("vct_2024/matches/overview.csv", stringsAsFactors = FALSE)

ovb <- subset(ov, Side == "both")


mvps_por_mapa <- ov %>%
    filter(Side == "both", Map != "All Maps") %>%
    group_by(Map) %>%
    slice_max(order_by = Average.Combat.Score, n = 1, with_ties = FALSE) %>%
    select(Map, Player, Average.Combat.Score)

ruta_jpg <- "graficos/MVPXmapa.jpg"

jpeg(ruta_jpg, width = 800, height = 600, quality = 100)

ggplot(mvps_por_mapa, aes(x = reorder(Map, Average.Combat.Score), y = Average.Combat.Score)) +
    geom_segment(aes(xend = Map, y = 0, yend = Average.Combat.Score), color = "#999999") +
    geom_point(aes(color = Player), size = 5) +
    coord_flip() +
    labs(title = "MVPs por Mapa (Lollipop Chart)",
        x = "Mapa",
        y = "Average Combat Score") +
    theme_minimal()
dev.off()