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

remove_outliers <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    x[x >= lower & x <= upper]
}

for (var in variables) {
    cat("\n-------------------------------\n")
    cat("Variable:", var, "\n")

    x <- na.omit(df[[var]])
    if (length(x) < 3 || length(x) > 5000) {
        cat("No se puede aplicar Shapiro (n =", length(x), ")\n")
        next
    }
    p1 <- shapiro.test(x)$p.value
    cat("• p-valor original:", round(p1, 4), ifelse(p1 < 0.05, "No normal", "Normal"), "\n")

    x_no_out <- remove_outliers(x)
    p2 <- shapiro.test(x_no_out)$p.value
    cat("• p-valor sin outliers:", round(p2, 4), ifelse(p2 < 0.05, "No normal", "Normal"), "\n")

    if (p2 < 0.05) {
        x_trans <- log(x_no_out + 1)
        p3 <- shapiro.test(x_trans)$p.value
        cat("• p-valor log(x+1):", round(p3, 4), ifelse(p3 < 0.05, "No normal", "Normal"), "\n")
    }
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
    cat("\n-------------------------------\n")
    cat("Variable:", var, "\n")

    x <- na.omit(df_eco[[var]])
    if (length(x) < 3 || length(x) > 5000) {
        cat("No se puede aplicar Shapiro (n =", length(x), ")\n")
        next
    }

    p1 <- shapiro.test(x)$p.value
    cat("• p-valor original:", round(p1, 4), ifelse(p1 < 0.05, "No normal", "Normal"), "\n")

    x_no_out <- remove_outliers(x)
    p2 <- shapiro.test(x_no_out)$p.value
    cat("• p-valor sin outliers:", round(p2, 4), ifelse(p2 < 0.05, "No normal", "Normal"), "\n")

    # Transformación si sigue sin ser normal
    if (p2 < 0.05) {
        x_trans <- log(x_no_out + 1)
        p3 <- shapiro.test(x_trans)$p.value
        cat("• p-valor log(x+1):", round(p3, 4), ifelse(p3 < 0.05, "No normal", "Normal"), "\n")
    }
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

remove_outliers <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    x[x >= lower & x <= upper]
}

# Análisis paso a paso
cat("\n-------------------------------\n")
cat("Variable: DurationSeconds\n")

x <- na.omit(df_maps$DurationSeconds)
    if (length(x) < 3 || length(x) > 5000) {
        cat(" No se puede aplicar Shapiro (n =", length(x), ")\n")
        } else {
        p1 <- shapiro.test(x)$p.value
        cat("• p-valor original:", round(p1, 4), ifelse(p1 < 0.05, "No normal", "Normal"), "\n")


    x_no_out <- remove_outliers(x)
    p2 <- shapiro.test(x_no_out)$p.value
    cat("• p-valor sin outliers:", round(p2, 4), ifelse(p2 < 0.05, "No normal", "Normal"), "\n")

    # Transformación si sigue sin ser normal
    if (p2 < 0.05) {
        x_trans <- log(x_no_out + 1)
        p3 <- shapiro.test(x_trans)$p.value
        cat("• p-valor log(x+1):", round(p3, 4), ifelse(p3 < 0.05, "No normal", "Normal"), "\n")
    }
}