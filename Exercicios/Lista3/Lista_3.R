# Integrantes:
#
# nome: Aristeu Alves Ferreira Neto
# matricula: 11911BCC027
#
# nome: Gabriel Riquieri Campos 
# matricula: 11911BCC030
#
# nome: Paulo Kiyoshi Oyama Filho
# matricula: 11911BCC022

#Exericicio 1

#Sorteia o dados
dado <- c(1:6)
resultados <- c()

for(i in 1:10000){
  #Soma dos eh igual a 7 ou 11 
  resultados[i] <- sum(sample(dado, size=2, replace = T))
}
mean(resultados)
hist(resultados)

paste("A probabilidade do resultado ser 7 ou 11 eh  de ",mean(resultados == 7 | resultados == 11))



#Exercicio 2

#Urna 1,2,3 respectivamente
u1 <- c("P","P","P","P","P","P","B","B","B","V","V","V","V")
u2 <- c("P","P","P","B","B","B","B","B","V","V")
u3 <- c("P","P","P","P","B","B","V","V")
#Faz vetor dado
dado <- c(1:6)
n_sucessos <- 0

for(i in 1:100000){
  #Sorteia dado e verifica numero
  if(sample(dado,1,replace = T) == 5){
    #sorteia valor da urna correspondente
    valor_sorteado <- sample(u1,1, replace=T)
    #Eh vermelha?
    if(valor_sorteado == "V"){
      n_sucessos <- n_sucessos+1
    }
  }else if(sample(dado,1,replace = T) == 1 | sample(dado,1,replace = T) == 4 | sample(dado,1,replace = T) == 6){
    valor_sorteado <- sample(u2,1, replace=T)
    if(valor_sorteado == "V"){
      n_sucessos <- n_sucessos+1
    }
  }else{
    valor_sorteado <- sample(u3,1, replace=T)
    if(valor_sorteado == "V"){
      n_sucessos <- n_sucessos+1
    }
  }
}

paste("A probabilidade de se retirar uma bola vermelha eh ",n_sucessos/100000)



#Exercicio 3


dado <- c(1:6)
resultados <- c()
resultados2 <- c()
vitoria <- 0
derrota <- 0
flag <- T

somaLancamentos <- function(){
  #Gera dado
  dado <- c(1:6)
  #Soma os dois numeros obtidos
  resultado <- sum(sample(dado, size=2, replace = T))
  #Retorna resultado
  return(resultado)
}

for(i in 1:10000){
  flag <- T
  #Resultado recebe soma dos dados sorteados
  resultados[i] <- somaLancamentos()
 #Eh igual a 7 ou 11 ?
  if(resultados[i] == 7 | resultados[i] == 11){
    vitoria <- vitoria+1
    #Eh igual a 2,3 ou 12?
  }else if(resultados[i] == 2 | resultados[i] == 3 | resultados[i] == 12){
    derrota <- derrota+1
  }else{
    while(flag){
      #Gera um novo resultado
      resultados2[i] <- somaLancamentos()
      #Eh igual a 7?
      if(resultados2[i] == 7){
        derrota <- derrota+1
        flag<- F
        break
        #Ultimo resultado eh igual ao primeiro ? 
        }else if(resultados2[i] == resultados[i]){
        vitoria <- vitoria+1
        flag<- F
        break
      }
    }
  }
}

mean(vitoria/10000)



# Exercicio 4


issubsequence <- function(subsequencia, sequencia){ 
  if(length(subsequencia) > length(sequencia))
    return(FALSE)
  maximo <- length(sequencia) - length(subsequencia)
  
  for(i in 0:maximo){
    maior <- length(subsequencia)+i
    menor <- i+1
    res <- all(subsequencia == sequencia[menor:maior])
    if(res){
      return(TRUE)
    }
  }
  return(FALSE)
}

total <- 10000
contadorArya <- 0

for(i in 1:total) {
  res = c()
  repeat{
    sorteado <- sample(c(1,0), size=1, replace = T)
    res <- c(res, sorteado)
    if(issubsequence(c(0,0,1), res)){
      contadorArya <- contadorArya + 1
      break
    }else if(issubsequence(c(0,1,0), res)) {
      break
    }
  }
}

contadorArya/total



#Exercicio 5


#a) Sim, o que o Yoda disse está correto, já que se voce chamar de m, todas as vezes que deram cara e n, todas as vezes que deram coroa
# veja que não importa o número m ou n, para que o Luke esteja na origem é sempre necessário que m seja igual a n, caso
#não estão iguais, quando vc fizar m - n, expressão que determinar sua posição na reta (Ex: -1,0,2,3,-5,...), se o resultado é negativo
#entao ele mostrara quantas caras devem ser tiradas quando jogar a moeda para ir para a origem e se positivo quantas coroas 
# (Ex: m - n => 51 - 53 = -2, falta 2 caras para chegar na origem (51 +2) - 53 = 0). Portanto, se sempre que LUke esta na origem eh
#porque m eh igual a n, teremos que a soma de ambos m + n, nos dará o numero de jogadas ate agora, logo, m + n => m + m  => 2 * m, podemos 
# concluir que sempre que estivermos na origem o total de jogadas sera um multiplo de dois e por consequencia par.

numerosOrigem <- c()
result <- c()
j <- 1
for(i in 1:10000){
  
  sorteio <- sample(c(-1, 1), size = 1)
  if(i == 1){
    result[i] <- sorteio
  }else{
    result[i] <- result[i-1] + sorteio
    #Numero esta na origem
    if(result[i] == 0){
      #Guarda o valor do contador
      numerosOrigem[j] <- i
      j<- j + 1
    }
  }
}

#Proporcaos dos numeros pares que estao passaram na origem divido pelo total para saber a porcentagem
100*length(numerosOrigem[(numerosOrigem %% 2) == 0]) / length(numerosOrigem) 
#Como eh sempre 100 quer dizer que contando as jogadas para o LUke estar na origem sempre sera um numero par 



#b) 

result <- c()
cont1 <- 0
cont2 <- 0
cont3 <- 0
cont4 <- 0

for(j in 1:100000){
  for(i in 1:20){
    sorteio <- sample(c(-1, 1), size = 1)
    if(i == 1){
      result[i] <- sorteio
    }else{
      result[i] <- result[i-1] + sorteio
    }
    
  }
  if (result[4] == 0) {
    cont1 <- cont1 + 1
  }
  if (result[6] == 0) {
    cont2 <- cont2 + 1
  }
  if (result[10] == 0){
    cont3 <- cont3 + 1
  } 
  if (result[20] == 0) {
    cont4 <- cont4 + 1
  }
}

#i) 
100*cont1/100000

#ii) 
100*cont2/100000

#iii) 
100*cont3/100000

#iv) 
100*cont4/100000



#Exercicio 6


#a)
x <- runif(100000000, -1,2)
g <- (1/sqrt(2*pi)*exp(-x^2/2))
integral <- mean(g)
3*integral

#b)
x <- runif(100000, 0,pi)
g <- cos(x)^2
integral <- mean(g)
pi*integral


# Exercicio 7


exercicio7 <- function(n){ 
  result <- c()
  for(i in 1:n){
    x <- runif(1,0,1)
    if(x <= 1/3){
      result[i] <- 1
    }else{
      result[i] <- 2
    }
  }
  return(result)
}

n100 <- exercicio7(100)
n1000 <- exercicio7(1000)
n10000 <- exercicio7(10000)

mean(n100 == 2)
mean(n1000 == 2)
mean(n10000 == 2)



#Exercicio 8


probabilidade <- function(a, k){
  x <- c()
  for(i in 1:10000){
    j <- 1
    u <- runif(1, 0, 1)
    p <- choose(j-1,k-1)*(a^k)*((1-a)^(j-k))
    soma <- p
    repeat {
      if(u <= soma){
        x[i] <- j
        break
      }
      j <- j + 1
      soma <- soma + choose(j-1,k-1)*(a^k)*((1-a)^(j-k))
      p <- choose(j-1,k-1)*(a^k)*((1-a)^(j-k))
    }
  }
  return(x)
}

x <- probabilidade(4/7, 3)
mean(x > 8)



#Exercicio 9


#fazendo a inversa
# y = (x^2 + x)/2
# invertendo
# x = (y^2 + y)/2
# 0 = y^2 + y - 2x
# Delta = 1^2 - 4*1*(-2x)
# Delta = 1 + 8x
# (-1 +- sqrt(1+8x)) / 2

x <- runif(10000, 0, 1)
r <- (-1 + sqrt(1+8*x)) / 2
mean(r < 0.7)

