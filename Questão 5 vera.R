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

#(a) Ajustar o modelo de regressão linear simples
modelo <- lm(dados_gerados ~ horas_uso)

#Mostrar o resumo do modelo (aqui vemos o R², coeficientes, p-valor, etc.)
summary(modelo)

#(a) Mostrar os coeficientes da reta de regressão (intercepto e inclinação)
coef(modelo)
#Equação da reta: Dados Gerados = 121,75 + 38,02 × Horas de Uso
#(b) Interpretação do coeficiente angular:
#Para cada hora adicional de uso, a quantidade de dados gerados aumenta, em média,
#cerca de 38,02 unidades.
#(c) Prever a quantidade de dados gerados para 15 horas de uso
previsao <- 121.74512 + 38.01724 * 15
previsao

#(d) Avaliação do R²:
#O valor de R² é 0,6655, indicando que aproximadamente 66,55% da variação na quantidade de dados gerados
#O modelo é razoavelmente confiável.
#(e) Calcular o coeficiente de correlação entre horas de uso e dados gerados
cor(horas_uso, dados_gerados)
#Indica uma correlação forte e positiva entre as variáveis.