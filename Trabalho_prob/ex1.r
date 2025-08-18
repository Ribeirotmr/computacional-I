library(ggplot2)



#X ~ N(90,100) 
# 90 = MEDIA
# 100 = VARIANCIA , V^2 = 100 , Variancia = 10

#--------------------------Exercicio 3------------------------------#


#a) P(X <= 115)

solucao_a <- pnorm(115, mean = 90, sd = sqrt(100)) 
print(solucao_a)

#b) P(X >= 80)

solucao_b <- pnorm(80, mean = 90, sd = sqrt(100))
solucao_b <- 1 - solucao_b
print(solucao_b)

#c) P(X <= 75)

solucao_c <- pnorm(75, mean = 90, sd = sqrt(100))
print(solucao_c)

#d) P(80 <= X <= 110) 

solucao_d1 <- pnorm(110, mean = 90, sd = sqrt(100))
solucao_d2 <- pnorm(80, mean = 90, sd = sqrt(100))
solucao_d <- solucao_d1 - solucao_d2
print(solucao_d) 

#e) P(|X-90| <= 10)
# |X-90| <= 10 
#1° X - 90 <= 10 => X <= 100
#2° -(X - 90) <= 10, -X + 90 <= 10, -X <= -80, X >= 80

solucao_e1 <- pnorm(100, mean = 90, sd = sqrt(100))
solucao_e2 <- pnorm(80, mean = 90, sd = sqrt(100))
solucao_e <- solucao_e1 - solucao_e2
print(solucao_e)

#f) O valor de alfa tal que $P(90-alfa <= X <= 90+alfa) = gama, gamma=0.95

gama <- 0.95
alfa <- qnorm((1 + gama) / 2, mean = 90, sd = sqrt(100)) - 90
print(alfa)


#--------------------------Exercicio 4------------------------------#

# Faça os seguintes graficos 

#a) da função de densidade de uma variável com 
#distribuição de Poisson com parâmetro $\lambda = 5$


lambda <- 5
x <- 0:10

massa <- dpois(x, lambda)

barplot(massa, names.arg = x,
        main = "Função de Massa de Probabilidade (Poisson λ = 5)",
        xlab = "k", ylab = "P(X = k)",
        col = "lightblue", border = "blue")


#b) Da densidade de uma variavel X ~ N(90, 100)


media <- 90
desvio <- 10

x <- seq(60, 120, by = 0.1)

densidade <- dnorm(x, mean = media, sd = desvio)

plot(x, densidade, type = "l", lwd = 2,
     main = "Função de Densidade da Normal N(90, 100)",
     xlab = "x", ylab = "f(x)",
     col = "darkgreen")

#c) sobreponha ao gráfico anterior a densidade de uma variável 
#$Y \sim N(90, 80)$ e outra $Z \sim N(85, 100)$


x <- seq(50, 130, by = 0.1)


densidade_X <- dnorm(x, mean = 90, sd = sqrt(100))     
densidade_Y <- dnorm(x, mean = 90, sd = sqrt(80))      
densidade_Z <- dnorm(x, mean = 85, sd = sqrt(100))     

plot(x, densidade_X, type = "l", lwd = 2, col = "blue",
     ylim = c(0, max(densidade_X, densidade_Y, densidade_Z)),
     main = "Densidades de X ~ N(90,100), Y ~ N(90,80) e Z ~ N(85,100)",
     xlab = "x", ylab = "f(x)")


lines(x, densidade_Y, col = "red", lwd = 2, lty = 2)
     
lines(x, densidade_Z, col = "darkgreen", lwd = 2, lty = 3)

legend("topright", legend = c("X ~ N(90, 100)", "Y ~ N(90, 80)", "Z ~ N(85, 100)"),
       col = c("blue", "red", "darkgreen"), lwd = 2, lty = c(1, 2, 3))


#d) densidades de distribuições 
#$\chi^2$ com 1, 2 e 5 graus de liberdade.

x <- seq(0, 20, by = 0.1)

dens_1 <- dchisq(x, df = 1)
dens_2 <- dchisq(x, df = 2)
dens_5 <- dchisq(x, df = 5)


all_dens <- c(dens_1, dens_2, dens_5)
all_dens <- all_dens[is.finite(all_dens)]

plot(x, dens_1, type = "l", lwd = 2, col = "blue",
     ylim = c(0, max(all_dens)),
     main = "Densidades da distribuição χ² com 1, 2 e 5 graus de liberdade",
     xlab = "x", ylab = "f(x)")


lines(x, dens_2, col = "red", lwd = 2, lty = 2)
lines(x, dens_5, col = "darkgreen", lwd = 2, lty = 3)

legend("topright", legend = c("χ²(1)", "χ²(2)", "χ²(5)"),
       col = c("blue", "red", "darkgreen"), lwd = 2, lty = c(1, 2, 3))
