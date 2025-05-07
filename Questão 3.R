# 3° questão 

dados <-Base_de_dados_AV2

head(dados) #vizualisa os dados

#Diagrama de dispersão
plot(dados$Resíduos_sólidos_kg_dia, dados$IQA,
     main = "Resíduos Sólidos vs Índice de Qualidade da Água",
     xlab = "Resíduos Sólidos (kg/dia)",
     ylab = "Índice de Qualidade da Água (IQA)",
     pch = 19, col = "blue")

#Calcular o coeficiente de correlação
correlacao <- cor(dados$Resíduos_sólidos_kg_dia, dados$IQA)
cat("Coeficiente de correlação (r):", round(correlacao, 4), "\n")

#Criar modelo de regressão linear
modelo <- lm(IQA ~ Resíduos_sólidos_kg_dia , data = dados)

#Adicionar linha de regressão ao gráfico
abline(modelo, col = "red", lwd = 2)

#Resumo do modelo (inclui coeficientes, p-valor, R², etc.)
summary(modelo)

#Extração dos coeficientes e equação da reta
a <- coef(modelo)[1]  # Intercepto
b <- coef(modelo)[2]  # Inclinação
cat("Equação estimada: IQA =", round(a, 4), "+", round(b, 4), "* Resíduos\n")

#Coeficiente de determinação R²
r2 <- summary(modelo)$r.squared
cat("Coeficiente de Determinação (R²):", round(r2, 4), "\n")

