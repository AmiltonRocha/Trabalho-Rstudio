#Questão 1

x <- c(81,44,90,50,53,32,51,82,31,59, 67,31,93,89,50,62,87,51,78,88)
y <- c(2728.64, 1516.5, 3194.42, 1596.4, 1883.49, 1113.07, 2011.87, 2849.05, 979.98, 2447.55,1939.66, 1308.88, 3410.84, 2894.78, 1976.05, 2244.62, 2967.71, 1553.25, 2843.22, 2939.11)

#numerador
r1 = sum(x * y) - sum(x) * sum(y) / length(x)
#denominador
r2 = sqrt((sum(x^2) - (sum(x)^2 / length(x))) * (sum(y^2) - (sum(y)^2 / length(y))))
#coeficiente de correlação Amostral - Coeficiente de Correlação de Pearson
RXY=r1/r2

dados = data.frame(x,y) #criando um data.frame > is.data.frame(dados) #verifica se dados é um data.frame [1] TRUE >

cor(x,y)



#A)
plot(x, y, main = "Área útil x Aluguel",
     xlab = "Área útil (m²)",
     ylab = "Valor do aluguel (R$)")
grid()
points(x, 
       y, 
       pch = 20,
       col = "black")



#B)
regressão =lm(y~x)
summary(regressão)
abline(regressão, col = "red", lwd = 2)


#C)
novos_dados <- data.frame(x = c(90, 95, 100))
predict(regressão, novos_dados)


#D)
summary(regressão)$r.squared #Próximo de 1 = bom ajuste / Próximo de 0 = fraco ajuste 


#E)
summary(dados)
sd(dados$x)    # desvio padrão de X
var(dados$x)   # variância de X
sd(dados$y)    # desvio padrão de Y
var(dados$y)   # variância de Y