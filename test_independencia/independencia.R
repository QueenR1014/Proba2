library(tidyverse)


agents_pick_rates <- read_csv("agents_pick_rates.csv")
players_stats <- read_csv("players_stats.csv")
scores <- read_csv("scores.csv")


min_len <- min(nrow(agents_pick_rates), nrow(players_stats), nrow(scores))


df <- tibble(
  Map = as.factor(agents_pick_rates$Map[1:min_len]),
  Agent = as.factor(agents_pick_rates$Agent[1:min_len]),
  Player = as.factor(players_stats$Player[1:min_len]),
  Victory_Type = as.factor(scores$`Match Result`[1:min_len])
)


vars <- names(df)
pval_mat <- matrix(NA, nrow = length(vars), ncol = length(vars),
                   dimnames = list(vars, vars))

for (i in 1:(length(vars)-1)) {
  for (j in (i+1):length(vars)) {
    sub_df <- df %>% select(vars[i], vars[j]) %>% drop_na()
    tabla <- table(sub_df[[1]], sub_df[[2]])
    if (nrow(tabla) > 1 && ncol(tabla) > 1) {
      test <- suppressWarnings(chisq.test(tabla))
      pval <- test$p.value
    } else {
      pval <- NA
    }
    pval_mat[i, j] <- pval
    pval_mat[j, i] <- pval
  }
}


pval_df <- as.data.frame(pval_mat) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "p_value") %>%
  drop_na()


significativos <- pval_df %>%
  filter(p_value < 0.05) %>%
  arrange(p_value)

print("Pares con dependencia significativa (p < 0.05):")
print(significativos)

ggplot(pval_df, aes(Var1, Var2, fill = p_value)) +
  geom_tile() +
  geom_text(aes(label = round(p_value, 3)), size = 4) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(0, 0.05, 1)),
                       limits = c(0, 1), name = "p-valor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  ggtitle("Chi-cuadrado entre Map, Agent, Player y Tipo de victoria")
