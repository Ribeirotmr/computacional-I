set.seed(123) 

qtd_dados <- 2
qtd_simulacao <- 100000 
vetor_vencedor <- c(5,6,7,8,9)
vetor_perdeor <- c(11,12)

qtd_lancamentos <- numeric(qtd_simulacao) 
terminou_em_uma_jogada <- 0

for(i in 1:qtd_simulacao){
    vezes_lancados <- 1
    dados_lancados_soma <- sum(sample(1:6, qtd_dados, replace =T))

    if(dados_lancados_soma %in% vetor_vencedor){
        terminou_em_uma_jogada <- terminou_em_uma_jogada + 1
        qtd_lancamentos[i] <- vezes_lancados
        next
    }
    
    while(!(dados_lancados_soma %in% vetor_vencedor)){
        dados_lancados_soma <- sum(sample(1:6, qtd_dados, replace =T))
        vezes_lancados <- vezes_lancados + 1
    }

    qtd_lancamentos[i] <- vezes_lancados
}

media_dos_lancamentos <- mean(qtd_lancamentos)
probabilidade_de_uma_jogada <- terminou_em_uma_jogada / qtd_simulacao

cat("média lançamento jogador:", media_dos_lancamentos, "\n")
cat("probabilidade de terminar em uma jogada:", probabilidade_de_uma_jogada, "\n")

lucro_cassino <- 10 - (1.5 * qtd_lancamentos)
lucro_medio_cassino <- mean(lucro_cassino)

cat("lucro medio do cassino por jogador: $", round(lucro_medio_cassino, 2), "\n")
if(lucro_medio_cassino > 0){
    cat("O jogo é rentável para o cassino.\n")
} else {
    cat("O jogo NÃO é rentável para o cassino.\n")
}