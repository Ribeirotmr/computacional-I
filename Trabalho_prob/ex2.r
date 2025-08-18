# Exercicio 5.6 

# Aluno: Thierry Martins Ribeiro 

#----------------------------------------Exercicio 1----------------------------------------#

# Para binomial X ~ binomial(n = 20, p = 0.3) 


prob <- 0.3
eventos <- 20 

#a) P(X <= 3) 

sucesso_k_a <- 3 

calculo_binomial_a <- function() {
    soma <- 0
    for(i in 0:sucesso_k_a){
        combin_a <- choose(eventos, i)
        prob_a <- prob^(i)
        prob_a1 <- (1-prob)^(eventos - i)
        soma = soma + combin_a * prob_a * prob_a1
        
    }
    return(soma)
}

resultado <- calculo_binomial_a()
cat("Questão a) \n")
print(resultado)

#b) P(X = 3)

calcula_binomial_b <- function() {
    soma = 0
    combin_b <- choose(eventos, 3) 
    prob_b <- prob^(3)
    prob_b1 <- (1-prob)^(eventos-3)

    soma = soma + combin_b * prob_b * prob_b1

    return(soma)

}

resultado_b <- calcula_binomial_b()
cat("Questão b) \n")
print(resultado_b)

#c) P(5 < X ≤15)  

sucesso_k_b <- 15

calcula_binomial_c <- function() {
    soma <- 0
    for(i in 5:sucesso_k_b){
        combin_c <- choose(eventos, i)
        prob_c <- prob^(i)
        prob_c1 <- (1-prob)^(eventos - i)
        soma = soma + combin_c * prob_c * prob_c1
        
    }
    return(soma)
}

resultado_c <- calcula_binomial_c()
cat("Questão c) \n")
print(resultado_c)


#d) P(12 < X < 18) 


sucesso_k_d <- 17

calcula_binomial_d <- function() {
    soma <- 0
    for(i in 13:sucesso_k_d){
        combin_d <- choose(eventos, i)
        prob_d <- prob^(i)
        prob_d1 <- (1-prob)^(eventos - i)
        soma = soma + combin_d * prob_d * prob_d1
        
    }
    return(soma)
}

resultado_d <- calcula_binomial_d()
cat("Questão d) \n")
print(resultado_d)

#e) P(X > 10|X ≥3) 
# Parece ser uma probabilidade condicional, dado a probabilidade
# condicional dos valores maiores que 10 sabendo que x é maior ou igual a 3
# Nesse caso A = P(X > 10) -> P(10 < X <= 20)
# Para vamos fazer o mesmo B = P(X >= 3) -> P(3 <= X <= 20) 
# Uma observação que A já esta contido em B e a formula de P(A n B)/P(B) -> P(A)/P(B) 
# Para P(X > 10) -> P(10 < X <= 20)

sucesso_k_e <- 20

calcula_binomial_e <- function() {
    soma1 <- 0
    for(i in 11:sucesso_k_e){
        combin_e <- choose(eventos, i)
        prob_e <- prob^(i)
        prob_e1 <- (1-prob)^(eventos-i)
        soma1 = soma1 + combin_e * prob_e * prob_e1
    }
    return(soma1)
}

resultado_e1 <- calcula_binomial_e()



# Para P(X >= 3) -> P(3 <= X <= 20) 

calcula_binomial_e1 <- function() {
    soma2 <- 0
    for(i in 3:sucesso_k_e){
        combin_e11 <- choose(eventos, i)
        prob_e11 <- prob^(i)
        prob_e2 <- (1-prob)^(eventos-i)
        soma2 = soma2 + combin_e11 * prob_e11 * prob_e2
    }
    return(soma2)
    
}


resultado_e2 <- calcula_binomial_e1()

prob_condicional <- resultado_e1 / resultado_e2
cat("Questão e) \n")
print(prob_condicional)



#----------------------------------------Exercicio 2----------------------------------------#

# Para X ~ Poisson(λ =100) 
lambida <- 100

#a) P(50 < X ≤ 70)

final_a <- 70

poisson_a <- function() {
    soma1 <- 0
    for(i in 50:final_a){
        exp_a <- exp(-lambida) 
        la <- lambida^(i) 
        soma1 <- soma1 + (exp_a * la)/factorial(i)
    }
    return(soma1)
}

res_a <- poisson_a() 
cat("Questão a) \n")
print(res_a)



#b) P(X > 200) 

final_b <- 199 

poisson_b <- function() {
    soma2 <- 0
    for(i in 0:final_b){
        exp_b <- exp(-lambida) 
        lb <- lambida^(i) 
        soma2 <- soma2 + (exp_b * lb)/factorial(i)
    }
    return(soma2)
}

res_b <- poisson_b() 
calculo_b <- 1 - res_b
if(is.nan(calculo_b)){
    calculo_b <- 0
}
cat("Questão b) \n")
print(calculo_b)

#c) P(X < 100|X > 50)

# 1° P(X < 100) 

final_c <- 99

poisson_c <- function(){
    soma3 <- 0
    for(i in 51:final_c) {
        exp_c <- exp(-lambida)
        lc <- lambida^(i)
        soma3 <- soma3 + (exp_c * lc)/factorial(i)
    }
    return(soma3)

}

res_c <- poisson_c() 

# 2° P(X > 50) 

final_c1 <- 50

poisson_c1 <- function() {
    soma3_1 <- 0
    for(i in 0:final_c1){
        exp_c1 <- exp(-lambida)
        lc1 <- lambida^(i)
        soma3_1 <- soma3_1 + (exp_c1 * lc1)/factorial(i)
    }
    return(soma3_1)
}

res_c1 <- 1 - poisson_c1()

prob_poisson_c <- res_c / res_c1 
cat("Questão c) \n")
print(prob_poisson_c)

#d) P(50 < X < 100) 

lambda <- 100

p_menor_100 <- ppois(99, lambda = lambda)

p_menor_igual_50 <- ppois(50, lambda = lambda)

p_entre <- p_menor_100 - p_menor_igual_50
cat("Questão d) \n")
print(p_entre)


#e) P(X ≤ x)=0.2

x <- qpois(0.2, lambda = lambda)
cat("Questão e) \n")
print(x)



#----------------------------------------Exercicio 3----------------------------------------#

#  Para X ~ Normal(µ =100, σ =10)

u <- 100
sigma <- 10

#a) P(µ−3σ ≥ X ≤ µ+3σ)

p_a1 <- pnorm(u + 3 * sigma, mean = u, sd = sigma)  
p_a2 <- pnorm(u - 3 * sigma, mean = u, sd = sigma)

p_a <- p_a1 - p_a2 
cat("Questão a) \n")
print(p_a) 


#b) P(X > x)=0.875 

p_b <- qnorm(1 - 0.875, mean = u, sd = sigma)
cat("Qestão b) \n")
print(p_b) 

#c) P(x1 < X < x2)=0.8 

p_c1 <- qnorm(0.1, mean = u, sd = sigma)
p_c2 <- qnorm(0.9, mean = u, sd = sigma)
cat("Questão c) \n")
print(p_c1)
print(p_c2)

#d) P(X > 50|X ≤100) 

p_d1 <- pnorm(100, mean = u, sd = sigma) 
p_d2 <- pnorm(50, mean = u, sd = sigma)

prob_condicional_d <- (p_d1 - p_d2)/p_d1
cat("Questão d) \n")
print(prob_condicional_d)


# e) P(X ≥50|X ≤100) 

p_e1 <- pnorm(50, mean = u, sd = sigma)  
p_e2 <- pnorm(100, mean = u, sd = sigma)

prob <- 1 - (p_e1)/ p_e2

cat("Questão e) \n")
print(prob)












