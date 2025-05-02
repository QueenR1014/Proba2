library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

datos <- read_csv("overview.csv")

vc2024 <- datos %>% 
  filter(Tournament == "Valorant Champions 2024")

vc2024 <- vc2024 %>%
  filter(!str_detect(Agents, ","))  

acs_promedio <- vc2024 %>%
  group_by(Map, Agents) %>%
  summarise(ACS = mean(`Average Combat Score`, na.rm = TRUE), .groups = "drop")

todos_los_combos <- expand.grid(
  Map = unique(vc2024$Map),
  Agents = unique(vc2024$Agents)
)

acs_completo <- left_join(todos_los_combos, acs_promedio, by = c("Map", "Agents")) %>%
  replace_na(list(ACS = NA))


ggplot(acs_completo, aes(x = Agents, y = Map, fill = ACS)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(ACS), "", round(ACS, 1))), color = "black", size = 3) +
  scale_fill_gradient(
    low = "#f8cdd6",   
    high = "#c2185b",  
    na.value = "white"
  ) +
  labs(title = "Promedio de ACS por Agente Individual y Mapa",
       x = "Agente", y = "Mapa", fill = "ACS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
