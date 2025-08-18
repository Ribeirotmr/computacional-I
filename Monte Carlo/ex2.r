set.seed(123)
n <- 10000 #Numero de pontos
qtd_simulacao <- 10
a <- 0.1
b <- 50
f <- function(x) 500 * abs(sin(x) * cos(sqrt(x)) + log(x + 1) * exp(-x/20))

estimativa_f <- numeric(qtd_simulacao)
estimativa_i <- numeric(qtd_simulacao) 


for(i in 1:qtd_simulacao) {
    x_n <- runif(n, min = a, max = b)
    estimativa_f[i] <- integrate(f, lower = a, upper = b)$value
    estimativa_i[i] <- (b-a) * mean(f(x_n))
}

cat("Estimativa F\n")
print(estimativa_f)

cat("Estimativa para I\n")
print(estimativa_i)