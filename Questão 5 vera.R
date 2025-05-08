#Questão 5
horas_uso <- c(
  9.30, 7.29, 10.64, 6.51, 10.23, 11.28, 11.60, 13.52, 11.39, 13.03,
  7.75, 11.39, 10.81, 11.56, 11.33, 9.07, 11.14, 9.89, 11.71, 12.05,
  8.56, 10.51, 13.27, 9.67, 11.27, 9.85, 10.51, 7.27, 11.75, 8.41,
  11.01, 13.14, 7.19, 9.07, 10.06, 7.77, 8.41, 13.59, 10.17, 11.94,
  10.00, 8.03, 11.90, 8.10, 10.36, 8.81, 11.54, 12.21, 9.22, 10.24
)

dados_gerados <- c(
  450.33, 435.26, 523.41, 342.09, 546.96, 546.33, 475.77, 672.48, 601.29, 590.78,
  353.43, 493.22, 554.44, 615.11, 560.06, 560.20, 501.33, 417.72, 479.08, 632.82,
  426.16, 511.56, 647.84, 546.08, 465.61, 467.58, 567.70, 396.92, 583.92, 466.89,
  547.91, 610.61, 418.77, 507.22, 463.82, 463.89, 503.61, 761.99, 491.29, 490.79,
  521.97, 352.20, 580.84, 408.47, 552.99, 490.31, 570.20, 539.18, 470.39, 499.48
)

#Ajuste do modelo
modelo <- lm(dados_gerados ~ horas_uso)

# Plotar gráfico de dispersão
plot(horas_uso, dados_gerados,
     main = "Horas de Uso x Dados Gerados",
     xlab = "Horas de Uso por Semana",
     ylab = "Dados Gerados (MB)",
     pch = 19,
     col = "blue")

# Adicionar linha de regressão
abline(modelo, col = "red", lwd = 2)

# a) Equação da regressão
intercepto <- coef(modelo)[1]
inclinacao <- coef(modelo)[2]
cat("a) Equação da reta:\n")
cat("Y = ", round(intercepto, 2), "+", round(inclinacao, 2), "* X\n\n")

# b) Interpretação do coeficiente angular
cat("b) Cada hora adicional de uso gera, em média,", round(inclinacao, 2), "MB a mais de dados.\n\n")

# c) Previsão para 15 horas semanais
prev_15 <- predict(modelo, data.frame(horas_uso = 15))
cat("c) Previsão para 15 horas de uso:", round(prev_15, 2), "MB\n\n")

# d) Avaliação do modelo
r2 <- summary(modelo)$r.squared
cat("d) R² =", round(r2, 4), "\n")
if (r2 > 0.7) {
  cat("O modelo é forte e confiável.\n\n")
} else if (r2 > 0.5) {
  cat("O modelo é moderadamente confiável.\n\n")
} else {
  cat("O modelo é fraco e pouco confiável.\n\n")
}

# e) Correlação entre X e Y
r <- cor(horas_uso, dados_gerados)
cat("e) Coeficiente de correlação (r):", round(r, 4), "\n")
