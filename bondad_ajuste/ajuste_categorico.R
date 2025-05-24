library(dplyr)
df <- read.csv("vct_2024/matches/overview.csv")
df <- df %>%
  filter(
    Tournament == "Valorant Champions 2024",
    Side == "both",
    Map != "All Maps"
  )

team_agent_counts <- df %>%
  group_by(Match.Name, Team) %>%
  summarise(Agents = list(Agents), .groups = "drop")


all_picked_agents <- unlist(team_agent_counts$Agents)

# Get observed counts (number of times each agent was picked across all teams)
observed_counts <- table(all_picked_agents)

#Probabilidades obtenidas de https://liquipedia.net/valorant/Patch_9.02/Statistics
expected_prob = c(
  astra = 0.065,
  breach = 0.286,
  brimstone = 0.111,
  chamber = 0.039,
  clove = 0.089,
  cypher = 0.443,
  deadlock = 0.052,
  fade = 0.171,
  gekko = 0.271,
  harbor = 0.038,
  iso = 0.048,
  jett = 0.485,
  kayo = 0.204,
  killjoy = 0.407,
  neon = 0.185,
  omen = 0.642,
  phoenix = 0.029,
  raze = 0.314,
  sage = 0.028,
  skye = 0.096,
  sova = 0.564,
  viper = 0.283,
  yoru = 0.06
)
#Probabilidades normalizadas
expected_prob = expected_prob / sum(expected_prob)

total_observed = sum(observed_counts)
expected_counts = expected_prob * total_observed

chisq.test(x = observed_counts, p = expected_prob, rescale.p = TRUE)
