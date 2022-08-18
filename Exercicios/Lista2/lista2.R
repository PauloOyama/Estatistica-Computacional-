#######  Exercício 1

# A) 

ex1A <- c(1:20)

# B) 

ex1B <- c(20:1)

# C)
aux1c <- c(1:20)
aux2c <- c(19:1)

ex1C <- c(aux1c,aux2c)

# D)

aux1D <- seq(from=3,to=36,by=3)
aux2D <- seq(from=1,to=34,by=3)

ex1D <- (0.1^aux1D)*(0.2^aux2D)

# E)

help(rep)

ex1E <- rep(c(4,6,3),10)

# F)

ex1F <- rep(c(4,6,3),length.out = 31)


#######  Exercício 2

aux <- seq(3, 6, by=0.1)
ex2 <- exp(aux)*cos(aux)


#######  Exercício 3

# A)
 
aux3 <- c(10:100)
result1 <- c(10:100)
result2 <- c(10:100)

for ( j in 1:91){
  result1[j] <- aux3[j]^3 
  result2[j] <- 4*aux3[j]^2 
}

ex3 <- sum(result1) + sum(result2)
# 26852735


# B)

aux3 <- c(10:25)
result1<- c(10:25)
result2<- c(10:25)
for ( j in 1:16){
  result1[j] <- (2^aux3[j])/aux3[j]
  result2[j] <- (3^aux3[j])/aux3[j]^2
} 

ex3B <- sum(result1) + sum(result2)
#2129169868.47566

#######  Exercício 4

xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

# A)

vetA <- xVec[xVec%%2 != 0]

# B)

vetB <- yVec[2:250] - xVec[2:250 - 1]

 
# C)

vetC <-  sin(yVec[2:250])/ cos(xVec[2:250 - 1])

# D)

vetD <- xVec[3:250 - 2] + 2*xVec[3:250 - 1] - xVec[3:250] 

# E)

aux4e <- ((exp(1)/xVec[1:249])+1)/ (xVec[1:249] + 10)
vetE <- sum(aux4e)

#######  Exercício 5

# A)

result5a <- which(yVec[1:250] > 600)

# B)

result5b <- yVec[yVec[1:250] > 600]

# C)

result5c <- xVec[which(yVec > 600)]

# D)

media <- mean(xVec)

result5d <- sqrt(abs(xVec[1:250] - media))

# E)
  
result5e <- sum((max(yVec) - yVec[1:250]) <=  200 )

# F) 

result5f <- sum(xVec[1:250]%%2 == 0)

# G)

aux5G <- sort(yVec)

result5g <- c()

for(i in 1:250){
    result5g[i] <- which(yVec[1:250] == aux5G[i])
}

# H)

aux5h <- seq(from=1, to = 250, by= 3)

result5h <- yVec[aux5h] 


# 6)

recursaoTermos <- function(x) {
  if(x==0){ 
    return (0) 
    }else
  if(x==1){
    return (2/3)
  }else{
    return ( ( (2*x)/(2*x + 1) )  * ( recursaoTermos (x-1) ) )
  }
}

recursaoGeral <- function(x) {
  if (x == 1){
    return (1)
  }
  else {
    return ( recursaoTermos(x-1)  + recursaoGeral (x-1) ) 
  }      
}

#f(1) = 1
#f(2) = f(1) + 2/3
#f(3) = f(2) + ( g(1) * 4/5 )

#f() == recursaoGeral
#g() == recursaoTermos

result6 <- recursaoGeral(19)

#7)

library(dslabs)
data(murders)

#a)

media <- mean(murders$population)

estadoMaisPopuloso <- murders$state[order(murders$population)[length(murders$population)]]


estadoMenosPopuloso <- murders$state[order(murders$population)[1]]


#b)

murders$taxa  <- murders$total * 10000/murders$population


#c) 

mortalidade <- murders$state[order(murders$taxa)]


#d)

maiorMortalidade <- mortalidade[length(mortalidade)]

menorMortalidade <- mortalidade[1]

#e)

murders$region

#SOUTH
regiaoSouth <- murders$taxa[murders$region == "South"]

mediaSouth <- mean(regiaoSouth)

somaSouth <- sum((regiaoSouth - mediaSouth)^2)
desvioSouth <- sqrt(  somaSouth / length(regiaoSouth) )


#WEST
regiaoWest <- murders$taxa[murders$region == "West"]

mediaWest <- mean(regiaoWest)

somaWest <- sum((regiaoWest - mediaWest)^2)
desvioWest <- sqrt(  somaWest / length(regiaoWest) )


#NORTHEAST

regiaoNortheast <- murders$taxa[murders$region == "Northeast"]

mediaNortheast <- mean(regiaoNortheast)

somaNortheast <- sum((regiaoNortheast - mediaNortheast)^2)
desvioNortheast <- sqrt(  somaNortheast / length(regiaoNortheast) )

#NORTH CENTRAL

regiaoNorthCentral <- murders$taxa[murders$region == "North Central"]

mediaNorthCentral <- mean(regiaoNorthCentral)

somaNorthCentral <- sum((regiaoNorthCentral - mediaNorthCentral)^2)
desvioNorthCentral <- sqrt(  somaNorthCentral / length(regiaoNorthCentral) )



taxaPorRegiao <- c(mediaNorthCentral,mediaNortheast,mediaSouth,mediaWest)

auxMortalidade <- order(taxaPorRegiao)


auxMaiorMortalidadePorRegiao <- auxMortalidade[length(auxMortalidade)]
auxMenorMortalidadePorRegiao <- auxMortalidade[1]

whichRegion <- function(auxRegion){
  
if(auxRegion == 1){
  return ("North Central")
}else if(auxRegion == 2){
  return ("Northeast")
  
}else if(auxRegion ==3){
  return ("South")
}else {
  return ("West")
}
}

maiorRegiaoMortalidade <- whichRegion(auxMaiorMortalidadePorRegiao)
menorRegiaoMortalidade <- whichRegion(auxMenorMortalidadePorRegiao)

#f)

boxplotSouth = murders$taxa[ murders$region == "South"]
boxplotWest = murders$taxa[ murders$region == "West"]
boxplotNortheast = murders$taxa[ murders$region == "Northeast"]
boxplotNorthCentral = murders$taxa[ murders$region == "North Central"]

namesRegion <- c("North Central", "Northeast","South","West")

boxplot(boxplotNorthCentral,boxplotNortheast,boxplotSouth,boxplotWest, names = namesRegion)

#PODE-SE OBSERVAR PELO PLOT DOS BLOXPLOT QUE A REGIAO SOUTH TEM A MAIOR TAXA DE MORTALIDADE POR 10 MIL HABITANTES CONTUDO
#POSSUI A MENOR DAS DISPERSOES  DENTRE OS 4 BOXPLOTS, POSSUINDO UMA ASSIMETRIA POSITIVA JA QUE A MEDIANA SE APROXIMA MAIS DO PRIMEIRO QUARTIL,
#ESTA MEDIANA ESTA REPRESENTADO PELO ESTADO DA FLORIDA E POSSUINDO ALEM DISSO DOIS OUTLIERS QUE SAO RESPECTIVAMENTE OS ESTADOS DE LOUISIANA E DISTRICT OF COLUMBIA
#A REGIAO WEST POSSUI ASSIMETRIA POSITIVA TAMBEM  COM A MAIOR  DISPERSAO DOS SEUS VALORES COM MEDIANA REPRESENTADA PELO ESTADO DO COLORADO,
#JA A SUA TAXA DE MORTALIDADE E QUASE IGUAL A DA REGIAO NORTH CENTRAL E
#NORTHEAST,  QUE POR SUA VEZ POSSUEM PRATICAMENTE A MESMA DISPERSAO POREM OQUE AS DIFERE SAO OS SEUS LIMITES SUPERIORES TENDO A NORTH CENTRAL UM LIMITE
#MAIOR DO QUE A DE  NORTHEAST. ALEM DISSO AMBAS POSSUEM UMA ASSIMETRIA BEM DISTRIBUIDA COM A MEDIANA QUASE NO CENTRO E SENDO A MEDIANA DE NORTHEAST
#REPRESENTADA POR PELO ESTADO DE MASSACHUSETTS ENQUANTO QUE A MEDIANA DE NORTH CENTRAL EH REPRESENTADA PELO VALOR 0.1971105 VISTO QUE QUE SEU TAMANHO EH IMPAR

