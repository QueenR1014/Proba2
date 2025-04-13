# Cargar paquetes
library(dplyr)

# Leer datos
df <- read.csv("vct_2024/agents/teams_picked_agents.csv") # Reemplaza por el nombre correcto

# Filtrar solo partidas reales (excluir "All Stages")
df_filtrado <- df %>% filter(Stage != "All Stages")
df_filtrado = subset(df_filtrado, Tournament == "Valorant Champions 2024")

# Crear una columna que identifique el partido (mapa + equipos)
df_filtrado <- df_filtrado %>%
  mutate(Partido_ID = paste(Stage, Match.Type, Map, Team, sep = "_"))

# Obtener todos los partidos únicos (agrupando por Stage, Match.Type y Map)
partidos <- df_filtrado %>%
  group_by(Stage, Match.Type, Map) %>%
  summarise(Jugadores = n(), .groups = "drop") %>%
  filter(Jugadores == 10)  # Asegurarse de que sea un partido completo (5v5)

# Unir con los registros originales para quedarnos solo con los partidos válidos
df_validos <- df_filtrado %>%
  semi_join(partidos, by = c("Stage", "Match.Type", "Map"))

# Contar en cuántos partidos se seleccionó Omen o Viper
conteo_seleccion <- df_validos %>%
  filter(Agent %in% c("omen", "viper")) %>%
  group_by(Stage, Match.Type, Map, Agent) %>%
  summarise(Escogido = 1, .groups = "drop")

# Crear base con todos los partidos posibles para cada agente
partidos_unicos <- df_validos %>%
  distinct(Stage, Match.Type, Map)

# Expandir para ambos agentes
partidos_agente <- partidos_unicos %>%
  tidyr::crossing(Agent = c("omen", "viper"))

# Unir con conteo de selección para tener 0 si no fue escogido
seleccion_completa <- partidos_agente %>%
  left_join(conteo_seleccion, by = c("Stage", "Match.Type", "Map", "Agent")) %>%
  mutate(Escogido = ifelse(is.na(Escogido), 0, Escogido))

# Calcular totales
tabla_final <- seleccion_completa %>%
  group_by(Agent) %>%
  summarise(
    Veces_Escogido = sum(Escogido),
    Total_Partidos = n()
  )

print(tabla_final)

# Test de diferencia de proporciones
# p1 = Omen, p2 = Viper
x1 <- tabla_final$Veces_Escogido[tabla_final$Agent == "omen"]
n1 <- tabla_final$Total_Partidos[tabla_final$Agent == "omen"]

x2 <- tabla_final$Veces_Escogido[tabla_final$Agent == "viper"]
n2 <- tabla_final$Total_Partidos[tabla_final$Agent == "viper"]

# Prueba Z para diferencia de proporciones
prop.test(x = c(x1, x2), n = c(n1, n2), correct = FALSE)
#ayuda
