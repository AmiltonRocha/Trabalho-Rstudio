#questão 4: 
# Análise de regressão linear simples

# Dados fornecidos
n <- 20
soma_x <- 1478
soma_y <- 12.75
soma_xy <- 1083.67
soma_x2 <- 143215.8

# a) Cálculo dos parâmetros da regressão

# Coeficiente angular (b)
Coeficiente_angular <- (n * soma_xy - soma_x * soma_y) / (n * soma_x2 - soma_x^2)

# Coeficiente linear (a)
Coeficiente_linear <- (soma_y - Coeficiente_angular * soma_x) / n


# Exibir os coeficientes
cat("=== a) Parâmetros da regressão ===\n")
cat("Coeficiente angular (b):", round(Coeficiente_angular, 4), "\n")
cat("Coeficiente linear (a):", round(Coeficiente_linear, 4), "\n")
cat("Equação estimada: y =", round(Coeficiente_linear, 4), "+", round(Coeficiente_angular, 4), "* x\n\n")



# b) Gráfico da equação ajustada

# Criar vetor de valores de x (temperaturas simuladas)
x <- seq(60, 100, by = 1)

# Calcular valores estimados de y
y <- Coeficiente_linear + Coeficiente_angular * x

# Gerar o gráfico
plot(x, y,
     type = "l", col = "blue", lwd = 2,
     main = "Temperatura vs Deformação da Pavimentação",
     xlab = "Temperatura Superficial (°F)",
     ylab = "Deformação da Pavimentação")
legend("topleft",
       legend = paste0("ŷ = ", round(Coeficiente_linear, 4), " + ", round(Coeficiente_angular, 4), "x"),
       bty = "n")



# c) Estimativa da deformação para 85°F

f <- 85
y_estimado <- Coeficiente_linear + Coeficiente_angular * f

cat("=== c) Estimativa para 85°F ===\n")
cat("Deformação estimada para 85°F:", round(y_estimado, 4), "\n\n")


# d) Variação esperada por 1°F
cat("=== d) Variação esperada por 1°F ===\n")
cat("A cada aumento de 1°F, espera-se uma mudança de", round(Coeficiente_angular, 4), "na deformação da pavimentação.\n")


