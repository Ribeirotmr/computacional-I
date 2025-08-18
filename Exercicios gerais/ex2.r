#Link para os exercícios:

url <-"http://cursos.leg.ufpr.br/ecr/probabilidade-e-vari%C3%A1veis-aleat%C3%B3rias.html#exerc%C3%ADcios-17"

#----------------------------------------Exercicio 1----------------------------------------#


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

#d) P(85 <= X <= 110) 

solucao_d1 <- pnorm(110, mean = 90, sd = sqrt(100))
solucao_d2 <- pnorm(85, mean = 90, sd = sqrt(100))
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


#----------------------------------------Exercicio 2----------------------------------------#

# Sendo X uma variável seguindo o modelo binomial com parâmetros n =15 e p=0.4 

# n <- 15
# p <- 0.4

#a) P(X >= 14)

p_a <- pbinom(13, size = n, prob = p)
p_aq <- 1 - p_a
cat("Questão a) \n")
print(p_a)

#b) P(8 < X <= 10) 

p_b1 <- pbinom(10, size = n, prob = p)
p_b2 <- pbinom(8, size = n, prob = p)
p_b <- p_b1 - p_b2
cat("Questão b) \n")
print(p_b)

#c) P(X < 2 ou  X >= 11) 

p_c1 <- pbinom(1, size = n, prob = p) 
p_c2 <- pbinom(10, size = n, prob = p) 
p_total_c <- p_c1 + (1-p_c2) 
cat("Questão c) \n")
print(p_total_c)

#d) P(X >= 11 ou X > 13) 

p_total_d <- 1 - pbinom(10, size = n, prob = p)
cat("Questão d) \n")
print(p_total_d)

#e) P(X > 3 ou X < 6)

p_e1 <- pbinom(3, size = n, prob = p)
p_e1_total <- 1 - p_e1 

p_e2 <- pbinom(5, size = n, prob = p)
p_intervalo <- p_e2 - p_e1

p_total_e2 <- p_e1_total + p_e2 - p_intervalo
cat("Questaõ e)\n")
print(p_total_e2)


#f) P(X <= 13 | X >= 11) 

p_f1 <- pbinom(10, size = n, prob = p)
p_f2 <- pbinom(13, size = n, prob = p) - p_f1 
p_f1_total <- 1 - p_f1 
p_total_f <- p_f2 / p_f1_total
cat("Questão f) \n")        
print(p_total_f)

#----------------------------------------Exercicio 3----------------------------------------#

#Uma empresa informa que 30% de suas contas a receber de outras empresas 
#encontram-se vencidas. Se o contador da empresa seleciona 
#aleatoriamente 5 contas, determine a probabilidade de:

n = 5 
p = 0.3

#a) Nenhuma conta vencida

p_a <- dbinom(0, size = n, prob = p)
cat("Questão a) \n")
print(p_a)

#b) Exatamente duas cotas vencidas 

p_b <- dbinom(2, size = n, prob = p)
cat("Questão b) \n")
print(p_b)

#c) Três ou mais contas estarem vencidas

p_c <- 1 - pbinom(2, size = n, prob = p)
cat("Questão c) \n")
print(p_c)


#----------------------------------------Exercicio 4----------------------------------------#


#Uma empresa recebe 720 emails em um intervalo de 8 horas.
#Qual a probabilidade de que

lambda <- (720 / 480)


#a) Em 6 minutos receba pelo menos 3 emails 

lambda * 6  # Taxa de emails por 6 minutos
p_a <- 1 - ppois(2, lambda = lambda)
cat("Questão a) \n")
print(p_a) 

#b) Em 4 minutos não receba nenhum email

p_b <- dpois(0, lambda = lambda * 4)
cat("Questão b) \n")
print(p_b)

#----------------------------------------Exercicio 5----------------------------------------#



#O processo de empacotamento de uma fábrica de cereais foi ajustado 
#de maneira que uma média de  
#μ = 13 kg de cereal seja colocado em cada caixa. 
#Sabe-se que existe uma pequena variabilidade no enchimento dos pacotes 
#devido à fatores aleatórios, e que o desvio-padrão 
#do peso de enchimento é de σ = 1kg. 
#Assume-se que a distribuição dos pesos tem distribuição normal. 
#Com isso, determine as probabilidades de que uma caixa escolhida ao acaso:

u = 13 
sigma = 1


#a) Pese entre 13,0 e 13,2 kg.

p_a <- pnorm(13.2, mean = u, sd = sigma) - pnorm(13, mean = u, sd = sigma)
cat("Questão a) \n")
print(p_a) 

#b) Tenha um peso maior do que 13,25 kg 

p_b <- 1 - pnorm(13.25, mean = u, sd = sigma)
cat("Questão b) \n")
print(p_b)

#c) Pese entre 12,8 e 13,1 kg

p_c <- pnorm(13.1, mean = u, sd = sigma) - pnorm(12.8, mean = u, sd = sigma)
cat("Questão c) \n")
print(p_c) 

#d) Pese entre 13,1 e 13,2 kg. 

p_d <- pnorm(13.2, mean = u, sd = sigma) - pnorm(13.1, mean = u, sd = sigma)
cat("Questão d) \n")
print(p_d)

#----------------------------------------Exercicio 6----------------------------------------#

#Faça os seguintes gráficos

#a) da função de densidade de uma variável com distribuição de Poisson com parâmetro λ=5


lambda <- 5
x <- 0:10

massa <- dpois(x, lambda)

barplot(massa, names.arg = x,
        main = "Função de Massa de Probabilidade (Poisson    = 5)",
        xlab = "k", ylab = "P(X = k)",
        col = "lightblue", border = "blue")


#b) da densidade de uma variável  X∼N(90,100).

media <- 90
desvio <- 10

x <- seq(60, 120, by = 0.1)

densidade <- dnorm(x, mean = media, sd = desvio)

plot(x, densidade, type = "l", lwd = 2,
     main = "Função de Densidade da Normal N(90, 100)",
     xlab = "x", ylab = "f(x)",
     col = "darkgreen")

#c) sobreponha ao gráfico anterior a densidade de uma variável  Y∼N(90,80) e outra Z∼N(85,100).


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



#d) densidades de distribuições  χ^2 com 1, 2 e 5 graus de liberdade.

x <- seq(0, 20, by = 0.1)

dens_1 <- dchisq(x, df = 1)
dens_2 <- dchisq(x, df = 2)
dens_5 <- dchisq(x, df = 5)


all_dens <- c(dens_1, dens_2, dens_5)
all_dens <- all_dens[is.finite(all_dens)]

plot(x, dens_1, type = "l", lwd = 2, col = "blue",
     ylim = c(0, max(all_dens)),
     main = "Densidades da distribuição X² com 1, 2 e 5 graus de liberdade",
     xlab = "x", ylab = "f(x)")


lines(x, dens_2, col = "red", lwd = 2, lty = 2)
lines(x, dens_5, col = "darkgreen", lwd = 2, lty = 3)

legend("topright", legend = c("χ²(1)", "χ²(2)", "χ²(5)"),
       col = c("blue", "red", "darkgreen"), lwd = 2, lty = c(1, 2, 3))
