df <- read.csv("vct_2024/players_stats/players_stats.csv")
df <- subset(df, Tournament == "Valorant Champions 2024" & Stage != "All Stages")
df_eco <- read.csv("vct_2024/matches/eco_rounds.csv")
df_eco <- subset(df_eco, Tournament == "Valorant Champions 2024" & Stage != "All Stages")
df_maps <- read.csv("vct_2024/matches/maps_scores.csv", stringsAsFactors = FALSE)
df_maps <- subset(df_maps, Tournament == "Valorant Champions 2024" & Stage != "All Stages")

df$`Kill..Assist..Trade..Survive..` <- as.numeric(gsub("%", "", df$`Kill..Assist..Trade..Survive..`))

df$`Headshot..` <- as.numeric(gsub("%", "", df$`Headshot..`))

variables <- c("Rating",
                "Average.Combat.Score",
                "Kill..Assist..Trade..Survive..",
                "Average.Damage.Per.Round",
                "Headshot..")

for (var in variables) {
    cat("Variable:", var, "\n")
    x <- df[[var]]
    x <- x[!is.na(x)]
    print(shapiro.test(x))
}

k <- function(x) {
    x <- gsub("k", "", x)
    x <- gsub(",", "", x)
    as.numeric(x) * 1000
}

df_eco$Loadout.Value <- k(df_eco$Loadout.Value)
df_eco$Remaining.Credits <- k(df_eco$Remaining.Credits)

df_eco$Loadout.Value <- as.numeric(df_eco$Loadout.Value)
df_eco$Remaining.Credits <- as.numeric(df_eco$Remaining.Credits)

vari <- c("Loadout.Value",
                "Remaining.Credits")

for (var in vari) {
    cat("Variable:", var, "\n")
    x <- df_eco[[var]]
    x <- x[!is.na(x)]
    print(shapiro.test(x))
}

t <- function(dur) {
    time_parts <- strsplit(dur, ":")
    sapply(time_parts, function(x) {
        h <- as.numeric(x[1])
        m <- as.numeric(x[2])
        s <- as.numeric(x[3])
        h * 3600 + m * 60 + s
    })
}
df_maps$DurationSeconds <- t(df_maps$Duration)

x <- na.omit(df_maps$DurationSeconds)
cat("Variable: Duration\n")
print(shapiro.test(x))

