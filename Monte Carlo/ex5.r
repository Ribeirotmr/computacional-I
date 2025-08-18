set.seed(123)

mu <- 0
sigma <- 1 

n <- 100
qtd_simulacao <- 100000

estimador_de_sigma2 <- numeric(qtd_simulacao)
estimador_de_s2 <- numeric(qtd_simulacao)

for(i in 1:qtd_simulacao){
    amostra_aleatoria <- rnorm(n, mean = 0, sd = sqrt(sigma)) 
    media <- mean(amostra_aleatoria)
    

    estimador_de_sigma2[i] <- (mean(amostra_aleatoria - media)^2)
    estimador_de_s2[i] <- sum((amostra_aleatoria)^2) / (n - 1)

}




EQM_estimador_sigma2 <- mean((estimador_de_s2 - sigma)^2)
EQM_estimador_s2 <- mean((estimador_de_s2 - sigma)^2)

cat("Estimador variacional com EQM: ", EQM_estimador_sigma2, "\n")
cat("Estimador amostral com EQM: ", EQM_estimador_s2, "\n")

