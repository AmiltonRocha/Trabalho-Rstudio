#Questão 2

# Dados
x <- c(1, 2, 3, 4, 5, 6, 7, 8,
       9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

y <- c(18.4286, 18.5704, 47.8500, 46.5819, 81.5325,
       95.3333, 67.7207, 114.3602, 82.9789, 159.5744,
       108.1095, 154.5456, 150.3914, 137.5996, 206.9332,
       173.6403, 153.1757, 129.8412, 201.8402, 281.1333)

#A) Faça o diagrama de dispersão;
plot(x, y,
     main = "Diagrama de Dispersão - Tamanho vs Tempo de Execução", #titulo do grafico
     xlab = "Tamanho da Entrada (milhares)", #eixo X titulo
     ylab = "Tempo de Execução (ms)", #eixo Y titulo
     pch = 19,#tipo de ponto que estou usando 
     col = "blue")

# Linha de tendência linear
modelo <- lm(y ~ x)
abline(modelo, col = "red", lwd = 2)


#B)Encontre a equação de regressão estimada;

#cirando modelo de regressão
modelo <- lm(y ~ x)

#Mostrando o resumo do modelo (coeficientes, R² etc.)
summary(modelo)

# Extraindo os coeficientes
a <- coef(modelo)[1]  # Intercepto
b <- coef(modelo)[2]  # Inclinação

#Mostrando a equação da reta
cat("Equação da reta estimada: ŷ =", round(a, 4), "+", round(b, 4), "* x\n")


#C)Calcule o coeficiente de determinação;

# Criar modelo de regressão
modelo <- lm(y ~ x)

#Mostrar resumo do modelo (com R²)
resumo <- summary(modelo)

#Extrair coeficientes
a <- coef(modelo)[1]  # Intercepto
b <- coef(modelo)[2]  # Inclinação

#Mostrar equação da reta
cat("Equação estimada: ŷ =", round(a, 4), "+", round(b, 4), "* x\n")

#Mostrar R²
r2 <- resumo$r.squared
cat("Coeficiente de Determinação (R²):", round(r2, 4), "\n")


#D)Determine análise descritiva dos dados

# Criando um data frame com os dados
dados <- data.frame(Tamanho = x, Tempo = y)

#Resumo estatístico geral
summary(dados)


#Média
mean_x <- mean(dados$Tamanho)
mean_y <- mean(dados$Tempo)

#Mediana
median_x <- median(dados$Tamanho)
median_y <- median(dados$Tempo)

#Mínimo e máximo
min_x <- min(dados$Tamanho)
max_x <- max(dados$Tamanho)
min_y <- min(dados$Tempo)
max_y <- max(dados$Tempo)


#Quartis
quantile_x <- quantile(dados$Tamanho)
quantile_y <- quantile(dados$Tempo)


#Desvio padrão
sd_x <- sd(dados$Tamanho)
sd_y <- sd(dados$Tempo)

#Variância
var_x <- var(dados$Tamanho)
var_y <- var(dados$Tempo)

#Amplitude (diferença entre max e min)
range_x <- range(dados$Tamanho)
range_y <- range(dados$Tempo)
amplitude_x <- diff(range_x)
amplitude_y <- diff(range_y)
