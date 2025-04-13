library(ggplot2)

pick <- read.csv("vct_2024/agents/agents_pick_rates.csv", stringsAsFactors = FALSE)

pick$O_Pick <- ifelse(pick$Agent == "Omen", "Omen", "Otro")
tabla_O <- table(pick$Map, pick$O_Pick)

chisq.test(tabla_O)

# Resultado del test de chi-cuadrado:
# X-squared = 5175.8, df = 9, p-value < 2.2e-16
#
# Conclusión:
# Dado que el p-valor es significativamente menor a 0.05, se rechaza la hipótesis nula.
# Esto indica que la proporción de selecciones de Omen depende significativamente del mapa.
# Es decir, Omen se pickea con mayor o menor frecuencia según el mapa, lo que sugiere 
# una preferencia táctica por parte de los equipos en ciertos entornos específicos del juego.