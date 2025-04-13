library(tidyverse)
library(wesanderson)


win_methods <- read_csv("win_loss_methods_count.csv")

win_methods_filtered <- win_methods %>%
  filter(Tournament == "Valorant Champions 2024")

total_wins_by_team <- win_methods_filtered %>%
  group_by(Team) %>%
  summarise(
    Elimination = sum(Elimination, na.rm = TRUE),
    Detonated = sum(Detonated, na.rm = TRUE),
    Defused = sum(Defused, na.rm = TRUE),
    `Time Expiry (No Plant)` = sum(`Time Expiry (No Plant)`, na.rm = TRUE),
    .groups = "drop"
  )

win_methods_long <- total_wins_by_team %>%
  pivot_longer(
    cols = c(Elimination, Detonated, Defused, `Time Expiry (No Plant)`),
    names_to = "Win_Type",
    values_to = "Count"
  )

most_common_by_team <- win_methods_long %>%
  group_by(Team) %>%
  filter(Count == max(Count, na.rm = TRUE)) %>%
  ungroup()

interpolated_palette <- colorRampPalette(wes_palette("GrandBudapest1"))(100)

ggplot(win_methods_long, aes(x = Win_Type, y = reorder(Team, -Count), fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Count == 0 | is.na(Count), "", Count)), color = "black", size = 3) +
  scale_fill_gradient(
    low = "#d1f2eb",  
    high = "#1abc9c", 
    name = "Cantidad"
  ) +
  labs(
    title = "Tipos de Victoria por Equipo",
    x = "Tipo de Victoria",
    y = "Equipo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )