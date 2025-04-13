library(tidyverse)
library(scales)
library(wesanderson)

scores <- read_csv("scores.csv")

champions_2024_scores <- scores %>%
  filter(Tournament == "Valorant Champions 2024") %>%
  mutate(played_rounds = `Team A Score` + `Team B Score`)

team_a_won <- champions_2024_scores %>%
  group_by(`Team A`) %>%
  summarise(Rounds_Won_A = sum(`Team A Score`, na.rm = TRUE),
            Rounds_Played_A = sum(played_rounds, na.rm = TRUE),
            .groups = "drop") %>%
  rename(Team = `Team A`)

team_b_won <- champions_2024_scores %>%
  group_by(`Team B`) %>%
  summarise(Rounds_Won_B = sum(`Team B Score`, na.rm = TRUE),
            Rounds_Played_B = sum(played_rounds, na.rm = TRUE),
            .groups = "drop") %>%
  rename(Team = `Team B`)

winrate_df <- full_join(team_a_won, team_b_won, by = "Team") %>%
  mutate(
    Rounds_Won_A = coalesce(Rounds_Won_A, 0),
    Rounds_Won_B = coalesce(Rounds_Won_B, 0),
    Rounds_Played_A = coalesce(Rounds_Played_A, 0),
    Rounds_Played_B = coalesce(Rounds_Played_B, 0),
    Total_Rounds_Won = Rounds_Won_A + Rounds_Won_B,
    Total_Rounds_Played = Rounds_Played_A + Rounds_Played_B,
    Win_Rate = Total_Rounds_Won / Total_Rounds_Played
  ) %>%
  select(Team, Total_Rounds_Won, Total_Rounds_Played, Win_Rate) %>%
  arrange(desc(Win_Rate))


base_colors <- wes_palette("FantasticFox1")
interpolated_palette <- colorRampPalette(base_colors)(23)

colors <- winrate_df %>%
  arrange(desc(Win_Rate)) %>%
  slice(1:23)

ggplot(colors, aes(x = reorder(Team, Win_Rate), y = Win_Rate, color = Team)) +
  geom_point(size = 4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = interpolated_palette) +
  labs(
    title = "Win Rate por Equipo",
    x = "Equipo",
    y = "Win Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

