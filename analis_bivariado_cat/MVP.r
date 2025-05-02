library(dplyr)
library(ggplot2)

ov <- read.csv("vct_2024/matches/overview.csv", stringsAsFactors = FALSE)

mvps_por_mapa <- ov %>%
    filter(Side == "both", Map != "All Maps") %>%
    group_by(Map) %>%
    slice_max(order_by = Average.Combat.Score, n = 1, with_ties = FALSE) %>%
    ungroup()

media <- mean(mvps_por_mapa$Average.Combat.Score, na.rm = TRUE)
mediana <- median(mvps_por_mapa$Average.Combat.Score, na.rm = TRUE)
sd_val <- sd(mvps_por_mapa$Average.Combat.Score, na.rm = TRUE)

lineas <- data.frame(
    valor = c(media, mediana, media + sd_val, media - sd_val),
    tipo = c("Media", "Mediana", "Media + SD", "Media - SD")
)

ruta_jpg <- "graficos/MVPXmapa.jpg"

jpeg(ruta_jpg, width = 800, height = 600, quality = 100)


ggplot(mvps_por_mapa, aes(x = reorder(Map, Average.Combat.Score), y = Average.Combat.Score)) +
    geom_segment(aes(xend = Map, y = 0, yend = Average.Combat.Score), color = "#999999") +
    geom_point(aes(color = Player), size = 5) +
    geom_hline(data = lineas, aes(yintercept = valor, linetype = tipo), color = "black") +
    scale_linetype_manual(values = c("dashed", "dashed", "dotted", "dotted")) +
    coord_flip() +
    labs(
    title = "MVPs por Mapa con Estadígrafos",
    x = "Mapa",
    y = "Average Combat Score",
    linetype = "Estadígrafos"
    ) +
    theme_minimal()
dev.off()