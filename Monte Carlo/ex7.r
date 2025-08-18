set.seed(123)
n <- 2 
qtd_simulacao <- 2000

vitoria <- 0

for(i in 1:qtd_simulacao ) {
    x_n <- sample(1:6, n, replace = T)
    estimativa <- sum(x_n)

    if(estimativa %% 3 == 0){
        vitoria <- vitoria + 1
    }

}

probabiliada_de_vitoria <- vitoria / qtd_simulacao 
cat("Estimativa de Ganhar:", probabiliada_de_vitoria, "\n")
