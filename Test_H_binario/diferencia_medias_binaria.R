library(dplyr)
df <- read.csv("vct_2024/matches/overview.csv")
df <- df %>%
  filter(
    Tournament == "Valorant Champions 2024",
    Side == "both",
    Map != "All Maps"
  )

#Data Frame con los registros de EG

df_EG = df %>% 
  filter(Team %in% c("EDward Gaming"))

df_headshot_EG = data.frame(Team = df_EG$Team, HS = df_EG$Headshot..)
df_headshot_EG$HS <- as.numeric(gsub("%", "", df_headshot_EG$HS)) / 100


#Data Frame con los registros de todos los otros equipos
df_others = df %>%
  filter(Team != "EDward Gaming")

df_headshot_others = data.frame(Team = df_others$Team, HS = df_others$Headshot..)
df_headshot_others$HS <- as.numeric(gsub("%", "", df_headshot_others$HS)) / 100

result_greater <- t.test(df_headshot_EG$HS, df_headshot_others$HS, alternative = "greater", conf.level = 0.95)
result_less <- t.test(df_headshot_EG$HS, df_headshot_others$HS, alternative = "less", conf.level = 0.95)
result_two_sided <- t.test(df_headshot_EG$HS, df_headshot_others$HS, alternative = "two.sided", conf.level = 0.95)
