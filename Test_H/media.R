df <- read.csv("vct_2024/matches/overview.csv")
df <- df %>%
  filter(
    Tournament == "Valorant Champions 2024",
    Side == "both",
    Map != "All Maps"
  )
library(dplyr)
df = df %>% 
  filter(Team %in% c("Team Heretics","EDward Gaming"))

headshot = data.frame(Team = df$Team, HS = df$Headshot..)
headshot$HS <- as.numeric(gsub("%", "", headshot$HS)) / 100

eg_hs <- headshot %>%
  filter(Team == "EDward Gaming") %>%
  pull(HS)

th_hs <- headshot %>%
  filter(Team == "Team Heretics") %>%
  pull(HS)

# Realizar la prueba t para comparar las medias
t_test_result <- t.test(eg_hs, th_hs, alternative = "less")

# Ver el resultado de la prueba t
t_test_result