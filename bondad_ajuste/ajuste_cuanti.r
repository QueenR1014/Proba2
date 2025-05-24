library(dplyr)
library(ggplot2)

df_kills <- read.csv("vct_2024/matches/kills.csv", stringsAsFactors = FALSE)
df_maps <- read.csv("vct_2024/matches/maps_scores.csv", stringsAsFactors = FALSE)

df_kills <- subset(df_kills, Tournament == "Valorant Champions 2024" &
                                Map != "All Maps" & Kill.Type == "All Kills")

df_maps <- subset(df_maps, Tournament == "Valorant Champions 2024")
df_maps$Rounds <- df_maps$Team.A.Score + df_maps$Team.B.Score
df_rondas <- df_maps[, c("Match.Name", "Map", "Rounds")]

df_kills <- merge(df_kills, df_rondas, by = c("Match.Name", "Map"))

df_agg <- df_kills %>%
    group_by(Match.Name, Map, Player.Team) %>%
    summarise(Kills = sum(Player.Kills, na.rm = TRUE),
            Rounds = unique(Rounds),
            .drop = "drop") %>%
    ungroup() %>%
    filter(!is.na(Kills), !is.na(Rounds), Rounds > 0)

lambda_hat <- sum(df_agg$Kills) / sum(df_agg$Rounds)

df_agg$Expected <- lambda_hat * df_agg$Rounds

df_agg$Class <- cut(df_agg$Kills,
                    breaks = c(35, 70, 80, 90, Inf),
                    labels = c("36–70", "71–80", "81–90", "≥91"))

df_agg$Expected.Class <- cut(df_agg$Expected,
                            breaks = c(35, 70, 80, 90, Inf),
                            labels = c("36–70", "71–80", "81–90", "≥91"))

observed <- table(df_agg$Class)

expected <- tapply(df_agg$Expected, df_agg$Expected.Class, sum)

expected[is.na(expected)] <- 0

common_classes <- intersect(names(observed), names(expected))
observed <- observed[common_classes]
expected <- expected[common_classes]

if (length(observed) > 0 && all(!is.na(expected)) && all(expected >= 0)) {
  expected <- expected * sum(observed) / sum(expected)
    resultado <- chisq.test(x = observed, p = expected / sum(expected))
    print(resultado)
}

