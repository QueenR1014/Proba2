tabla_map_agent <- table(df$Map, df$Agent)
test_map_agent <- chisq.test(tabla_map_agent)
residuos_map_agent <- test_map_agent$stdres

library(reshape2)
res_melt <- melt(residuos_map_agent)
colnames(res_melt) <- c("Map", "Agent", "Residual")

ggplot(res_melt, aes(Map, Agent, fill = Residual)) +
  geom_tile() +
  geom_text(aes(label = round(Residual, 2)), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  ggtitle("Residuos estandarizados: Map vs Agent")

