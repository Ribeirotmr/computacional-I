#Implemente o gerador Midsquare idealizado pelo matemático John von Neumann. 
#Por que o gerador Midsquare não é um bom gerador? Explique

# Ele nao e um bom gerador porque pode gerar sequências de números que não são uniformemente distribuídas,
# especialmente se a semente inicial não for escolhida adequadamente.
# O gerador Midsquare utiliza o quadrado da semente e extrai os dígitos do meio, o que pode levar a padrões repetitivos.
# Além disso, ele pode ter um período curto, o que significa que os números gerados podem se repetir rapidamente.



gerador_midsquare <- function(semente) {

  n <- as.integer(readline(prompt = "Quantos números aleatórios deseja gerar? "))
    
  numeros_novos <- numeric(n)
  
  for (i in 1:n) {
    semente <- semente^2

    semente_str <- sprintf("%08d", semente) 
    

    semente <- as.integer(substr(semente_str, 3, 6))
    
    numeros_novos[i] <- semente
  }
  
  return(numeros_novos)
}



semente_inicial <- sample(1000:9999, 1)
numeros_gerados <- gerador_midsquare(semente_inicial)
print(numeros_gerados)