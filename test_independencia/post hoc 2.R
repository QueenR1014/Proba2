
tabla_map_player <- table(df$Map, df$Player)

test_map_player <- chisq.test(tabla_map_player)

s
residuos_map_player <- test_map_player$stdres


library(reshape2)
res_melt_mp <- melt(residuos_map_player)
colnames(res_melt_mp) <- c("Map", "Player", "Residual")

ggplot(res_melt_mp, aes(Map, Player, fill = Residual)) +
  geom_tile() +
  geom_text(aes(label = round(Residual, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  ggtitle("Residuos estandarizados: Map vs Player") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
