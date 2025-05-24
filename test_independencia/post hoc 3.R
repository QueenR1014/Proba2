# Tabla de contingencia
tabla_map_victory <- table(df$Map, df$Victory_Type)

# Prueba Chi-cuadrado
test_map_victory <- chisq.test(tabla_map_victory)

# Extraer residuos estandarizados
residuos_map_victory <- test_map_victory$stdres

# Visualizar en heatmap
res_melt_mv <- melt(residuos_map_victory)
colnames(res_melt_mv) <- c("Map", "Victory_Type", "Residual")

ggplot(res_melt_mv, aes(Map, Victory_Type, fill = Residual)) +
  geom_tile() +
  geom_text(aes(label = round(Residual, 2)), size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  ggtitle("Residuos estandarizados: Map vs Victory_Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
