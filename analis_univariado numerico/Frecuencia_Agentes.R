library(tidyverse)
library(wesanderson)

teams_agents_df <- read_csv("teams_picked_agents.csv")

champions_2024_agents <- teams_agents_df %>%
  filter(Tournament == "Valorant Champions 2024", Stage == "All Stages") %>%
  count(Agent, sort = TRUE)

base_colors <- wes_palette("GrandBudapest2")
interpolated_palette <- colorRampPalette(base_colors)(23)

ggplot(champions_2024_agents, aes(x = reorder(Agent, -n), y = n, fill = Agent)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = interpolated_palette) +
  labs(title = "Frecuencia de Selección de Agentes",
       x = "Agente", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
