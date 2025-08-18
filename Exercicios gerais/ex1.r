library(stringr)

#link dos exercícios:
url <- "https://livro.curso-r.com/7-4-o-pacote-stringr.html#exerc%C3%ADcios-19 "


#4. Imagine que a seguinte string é a parte final de uma URL.
#/ac/rio-branco/xpto-xyz-1-0-1fds2396-5
#Transforme-a em “AC - Rio Branco” utilizando funções do pacote {stringr}.

url <- c('/ac/rio-branco/xpto-xyz-1-0-1fds2396-5') 

partes <- str_split(url, "/", simplify = TRUE)
estado <- str_to_upper(partes[2])
cidade <- str_replace_all(partes[3], "-", " ")
cidade <- str_to_title(cidade)
resultado <- str_c(estado, " - ", cidade)

print(resultado) 



#6. De acordo com as regras da língua portuguesa, antes de “p” ou “b” devemos usar a letra “m”. 
#Em outras palavras, com outras consoantes, usamos a letra “N”. 
#Suponha que você tem o seguinte texto com erros gramaticais:

texto <- 'Nós chamamos os bonbeiros quando começou o incêmdio.'
srt_bombeiros <- str_replace_all(texto, "bonbeiros", "bombeiros") 
srt_incendio <- str_replace_all(srt_bombeiros, "incêmdio", "incêndio")
print(srt_incendio)

#7. Considere o seguinte texto
#"A função mais importante para leitura de dados no `lubridate` é a `ymd`. 
#Essa função serve para ler qualquer data de uma `string` no formato `YYYY-MM-DD`. 
#Essa função é útil pois funciona com qualquer separador entre os elementos da data 
#e também porque temos uma função para cada formato (`mdy`, `dmy`, `dym`, `myd`, `ydm`)."
#Extraia todas as combinações da função ymd, sem repetições.

texto <- "A função mais importante para leitura de dados no `lubridate` é a `ymd`. Essa função serve para ler qualquer data de uma `string` no formato `YYYY-MM-DD`. Essa função é útil pois funciona com qualquer separador entre os elementos da data e também porque temos uma função para cada formato (`mdy`, `dmy`, `dym`, `myd`, `ydm`)."

funcoes <- str_extract_all(texto, "`[a-z]{3}`")[[1]]
funcoes_unicas <- unique(funcoes)
print(funcoes_unicas)




#8. Considere as frases abaixo
#Crie uma regra para identificar se o texto refere-se a um feedback positivo ou negativo 
#sobre o produto (considere “não bom = ruim” e “não ruim = bom”). 
#Retorne um vetor lógico que vale TRUE se o feedback é positivo e FALSE caso contrário.

s <- c(
  'O produto é muito bom.', #TRUE 
  'O produto não é bom.', #FALSE
  'O produto não é muito bom.', #FALSE
  'O produto não é ruim.', #TRUE
  'O produto não é não bom.' #FALSE
)

feedback <- str_detect(s, "não") & str_detect(s, "bom")
feedback_positivo <- !feedback  
print(feedback_positivo)

