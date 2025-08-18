set.seed(123)

qtd_simulacao <- 10000
numero_desejado <- 77
qtd_bolas_totais <- 100

retiradas <- numeric(qtd_simulacao)

for (i in 1:qtd_simulacao) {
  vezes <- 0
  bola_retirada <- 0
  
  while (bola_retirada != numero_desejado) {
    vezes <- vezes + 1
    bola_retirada <- sample(1:qtd_bolas_totais, 1, replace = TRUE)
  }
  
  retiradas[i] <- vezes
}

media_retiradas <- mean(retiradas)
probabilidade_de_desejada_mc <- mean(retiradas == 1)

cat("Media das bolas retiradas: ", media_retiradas, "\n")
cat("Probabilidade do desejo: ", probabilidade_de_desejada_mc, "\n")

